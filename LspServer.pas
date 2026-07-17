program LspServer;

{$mode objfpc}
{$longstrings on}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  sysutils, classes, ssockets, fpjson, jsonparser,
  ParserContext, Token, Identifier, Symbols, Scopes, ReservedWord, TypeDecl,
  ParameterDecl, TypeDefs, ProgramFile, UnitFile;

type
  TSemanticToken = record
    line: integer;
    position: integer;
    len: integer;
    tokenType: integer;
    tokenModifiers: integer;
  end;
  TSemanticTokenArray = array of TSemanticToken;

  TLspServerApp = class
  public
    procedure OnConnect(Sender: TObject; Data: TSocketStream);
  end;

var
  LastParsedUri: string = '';
  LastParserContext: TParserContext = nil;
  LastFileToken: TToken = nil;

function GetStdinHandle: THandle;
begin
  {$IFDEF WINDOWS}
  Result := GetStdHandle(STD_INPUT_HANDLE);
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function GetStdoutHandle: THandle;
begin
  {$IFDEF WINDOWS}
  Result := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ELSE}
  Result := 1;
  {$ENDIF}
end;

function ReadLine(Stream: TStream; var Line: string): boolean;
var
  ch: char;
  bytesRead: integer;
begin
  Line := '';
  repeat
    bytesRead := Stream.Read(ch, 1);
    if bytesRead = 1 then
    begin
      if ch = #10 then
        exit(true);
      if ch <> #13 then
        Line := Line + ch;
    end
    else
      exit(false);
  until false;
end;

procedure WriteStreamStr(Stream: TStream; const S: string);
begin
  if S <> '' then
    Stream.Write(S[1], Length(S));
end;

procedure SendResponse(WriteStream: TStream; const Payload: string);
var
  Header: string;
begin
  Header := 'Content-Length: ' + IntToStr(Length(Payload)) + #13#10 +
            'Content-Type: application/vscode-jsonrpc; charset=utf-8' + #13#10#13#10;
  WriteStreamStr(WriteStream, Header);
  WriteStreamStr(WriteStream, Payload);
end;

procedure FreeLastParsed;
begin
  if LastParserContext <> nil then
  begin
    LastParserContext.Free;
    LastParserContext := nil;
  end;
  LastFileToken := nil;
  TypesList.Clear;
  ResetScopes;
end;

procedure HandleFileChange(WriteStream: TStream; const Uri: string; const Content: string);
var
  ctx: TParserContext;
  fileToken: TToken;
  i: integer;
  cur: TToken;
  DiagnosticsJson: string;
  DiagCount: integer;
begin
  FreeLastParsed;
  
  ctx := TParserContext.Create(Content);
  if PeekReservedWord(ctx, rwUnit) then
    fileToken := TUnitFile.Create(ctx)
  else
    fileToken := TProgramFile.Create(ctx);
    
  LastParsedUri := Uri;
  LastParserContext := ctx;
  LastFileToken := fileToken;
  
  DiagnosticsJson := '';
  DiagCount := 0;
  
  for i := 0 to ctx.tokensLen - 1 do
  begin
    cur := ctx.Tokens[i];
    if cur.state in [tsError, tsMissing] then
    begin
      if DiagCount > 0 then
        DiagnosticsJson := DiagnosticsJson + ',';
      
      DiagnosticsJson := DiagnosticsJson + '{' +
        '"range":{' +
          '"start":{"line":' + IntToStr(cur.line) + ',"character":' + IntToStr(cur.position) + '},' +
          '"end":{"line":' + IntToStr(cur.line) + ',"character":' + IntToStr(cur.position + cur.len) + '}' +
        '},' +
        '"severity":1,'; // Error
      
      if cur.state = tsError then
        DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString(cur.errorMessage)) + '"'
      else
        if cur.GetStr() <> '' then
          DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString('Missing ''' + cur.GetStr() + '''')) + '"'
        else
          DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString('Missing ' + cur.tokenName)) + '"';
        
      DiagnosticsJson := DiagnosticsJson + '}';
      inc(DiagCount);
    end;
  end;
  
  SendResponse(WriteStream, 
    '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{' +
      '"uri":' + '"' + string(StringToJSONString(Uri)) + '"' + ',' +
      '"diagnostics":[' + DiagnosticsJson + ']' +
    '}}'
  );
end;

function IsInsideBlock(ident: TIdentifier; block: TToken): boolean;
begin
  Result := (ident <> nil) and (block <> nil) and 
            (ident.start >= block.start) and (ident.start < block.start + block.len);
end;

function IsMethodImplementation(symbol: TSymbol): boolean;
begin
  Result := (symbol.parent <> nil) and 
            (symbol.kind in [skProcedure, skFunction, skConstructor, skDestructor]) and
            ((symbol.parent.rangeToken = nil) or 
             (symbol.declaration.start < symbol.parent.rangeToken.start) or 
             (symbol.declaration.start >= symbol.parent.rangeToken.start + symbol.parent.rangeToken.len));
end;

function SerializeSymbol(symbol: TSymbol; interfaceBlock, implementationBlock: TToken): string;
var
  childJson, resultJson, symRange, selectionRange: string;
  symbolKindVal: integer;
  c: integer;
begin
  childJson := '';
  for c := 0 to length(symbol.children) - 1 do
  begin
    if (symbol.children[c] <> nil) and (symbol.children[c].declaration <> nil) then
    begin
      if not IsMethodImplementation(symbol.children[c]) then
      begin
        if childJson <> '' then
          childJson := childJson + ',';
        childJson := childJson + SerializeSymbol(symbol.children[c], interfaceBlock, implementationBlock);
      end;
    end;
  end;

  case symbol.kind of
    skUnitName: symbolKindVal := 4; // Package
    skTypeName: symbolKindVal := 5; // Class
    skConstant, skTypedConstant: symbolKindVal := 14; // Constant
    skVariable: symbolKindVal := 13; // Variable
    skProcedure, skFunction:
      begin
        if symbol.parent <> nil then
          symbolKindVal := 6 // Method
        else
          symbolKindVal := 12; // Function
      end;
    skConstructor, skDestructor: symbolKindVal := 9; // Constructor
  else
    symbolKindVal := 13; // Variable/Default
  end;

  if (symbol.rangeToken <> nil) and (symbol.rangeToken.endMarker <> nil) then
    symRange := '{"start":{"line":' + IntToStr(symbol.rangeToken.line) + ',"character":' + IntToStr(symbol.rangeToken.position) + '},' +
                '"end":{"line":' + IntToStr(symbol.rangeToken.endMarker.line) + ',"character":' + IntToStr(symbol.rangeToken.endMarker.position) + '}}'
  else
    symRange := '{"start":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position) + '},' +
                '"end":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position + symbol.declaration.len) + '}}';

  selectionRange := '{"start":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position) + '},' +
                     '"end":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position + symbol.declaration.len) + '}}';

  resultJson := '{' +
    '"name":' + '"' + string(StringToJSONString(symbol.displayName)) + '"' + ',' +
    '"kind":' + IntToStr(symbolKindVal) + ',' +
    '"range":' + symRange + ',' +
    '"selectionRange":' + selectionRange;

  if childJson <> '' then
    resultJson := resultJson + ',"children":[' + childJson + ']';

  resultJson := resultJson + '}';
  Result := resultJson;
end;

procedure HandleDocumentSymbol(WriteStream: TStream; const Uri: string; Id: TJSONData);
var
  Response: string;
  i, j, k: integer;
  scope: TScope;
  symbol: TSymbol;
  symRange: string;
  interfaceBlock, implementationBlock, curToken: TToken;
  interfaceChildren, implementationChildren, topLevelJson: string;
  symbolSerialized: string;
  isInsideInterface, isInsideImplementation: boolean;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';
    
  interfaceBlock := nil;
  implementationBlock := nil;
  interfaceChildren := '';
  implementationChildren := '';
  topLevelJson := '';
  
  if (LastParserContext <> nil) and (LastParsedUri = Uri) then
  begin
    for k := 0 to LastParserContext.tokensLen - 1 do
    begin
      curToken := LastParserContext.Tokens[k];
      if curToken.ClassNameIs('TInterfaceBlock') then
        interfaceBlock := curToken
      else if curToken.ClassNameIs('TImplementationBlock') then
        implementationBlock := curToken;
    end;

    for i := 0 to length(ScopesList) - 1 do
    begin
      if (i > 0) and ((LastFileToken = nil) or not (LastFileToken is TProgramFile) or (i > 1)) then
        continue;
      scope := ScopesList[i];
      for j := 0 to scope.symbolsList.Count - 1 do
      begin
        symbol := TSymbol(scope.symbolsList.Items[j]);
        if (symbol <> nil) and (symbol.declaration <> nil) then
        begin
          isInsideInterface := IsInsideBlock(symbol.declaration, interfaceBlock);
          isInsideImplementation := IsInsideBlock(symbol.declaration, implementationBlock);

          if isInsideInterface and (symbol.parent = nil) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if interfaceChildren <> '' then
              interfaceChildren := interfaceChildren + ',';
            interfaceChildren := interfaceChildren + symbolSerialized;
          end
          else if isInsideImplementation and ((symbol.parent = nil) or IsMethodImplementation(symbol)) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if implementationChildren <> '' then
              implementationChildren := implementationChildren + ',';
            implementationChildren := implementationChildren + symbolSerialized;
          end
          else if not isInsideInterface and not isInsideImplementation and ((symbol.parent = nil) or IsMethodImplementation(symbol)) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if topLevelJson <> '' then
              topLevelJson := topLevelJson + ',';
            topLevelJson := topLevelJson + symbolSerialized;
          end;
        end;
      end;
    end;
  end;

  if (interfaceBlock <> nil) and (interfaceBlock.endMarker <> nil) then
  begin
    symRange := '{"start":{"line":' + IntToStr(interfaceBlock.line) + ',"character":' + IntToStr(interfaceBlock.position) + '},' +
                '"end":{"line":' + IntToStr(interfaceBlock.endMarker.line) + ',"character":' + IntToStr(interfaceBlock.endMarker.position) + '}}';
    if topLevelJson <> '' then
      topLevelJson := topLevelJson + ',';
    topLevelJson := topLevelJson + '{' +
      '"name":"interface",' +
      '"kind":3,' + // Namespace
      '"range":' + symRange + ',' +
      '"selectionRange":' + symRange;
    if interfaceChildren <> '' then
      topLevelJson := topLevelJson + ',"children":[' + interfaceChildren + ']';
    topLevelJson := topLevelJson + '}';
  end;

  if (implementationBlock <> nil) and (implementationBlock.endMarker <> nil) then
  begin
    symRange := '{"start":{"line":' + IntToStr(implementationBlock.line) + ',"character":' + IntToStr(implementationBlock.position) + '},' +
                '"end":{"line":' + IntToStr(implementationBlock.endMarker.line) + ',"character":' + IntToStr(implementationBlock.endMarker.position) + '}}';
    if topLevelJson <> '' then
      topLevelJson := topLevelJson + ',';
    topLevelJson := topLevelJson + '{' +
      '"name":"implementation",' +
      '"kind":3,' + // Namespace
      '"range":' + symRange + ',' +
      '"selectionRange":' + symRange;
    if implementationChildren <> '' then
      topLevelJson := topLevelJson + ',"children":[' + implementationChildren + ']';
    topLevelJson := topLevelJson + '}';
  end;
  
  Response := Response + '"result":[' + topLevelJson + ']}';
  SendResponse(WriteStream, Response);
end;

procedure AddSemanticToken(var tokens: TSemanticTokenArray; var count, capacity: integer; line, pos, len, tType, tModifiers: integer);
begin
  if count >= capacity then
  begin
    capacity := capacity + 256;
    SetLength(tokens, capacity);
  end;
  tokens[count].line := line;
  tokens[count].position := pos;
  tokens[count].len := len;
  tokens[count].tokenType := tType;
  tokens[count].tokenModifiers := tModifiers;
  inc(count);
end;

procedure ScanComments(const Content: string; var tokens: TSemanticTokenArray; var count, capacity: integer);
var
  p: PChar;
  lineStart: PChar;
  line, pos: integer;
  commentStart: PChar;
  commentLen: integer;
begin
  if Content = '' then exit;
  p := PChar(Content);
  lineStart := p;
  line := 0;
  while p[0] <> #0 do
  begin
    if (p[0] = #13) and (p[1] = #10) then
    begin
      inc(p, 2);
      inc(line);
      lineStart := p;
    end
    else if p[0] in [#10, #13] then
    begin
      inc(p);
      inc(line);
      lineStart := p;
    end
    else if (p[0] = '/') and (p[1] = '/') then
    begin
      commentStart := p;
      pos := p - lineStart;
      inc(p, 2);
      while not (p[0] in [#10, #13, #0]) do
        inc(p);
      commentLen := p - commentStart;
      AddSemanticToken(tokens, count, capacity, line, pos, commentLen, 13, 0); // 13 = comment
    end
    else if p[0] = '{' then
    begin
      commentStart := p;
      pos := p - lineStart;
      inc(p);
      while (p[0] <> '}') and (p[0] <> #0) do
      begin
        if (p[0] = #13) and (p[1] = #10) then
        begin
          inc(p, 2);
          inc(line);
          lineStart := p;
        end
        else if p[0] in [#10, #13] then
        begin
          inc(p);
          inc(line);
          lineStart := p;
        end
        else
          inc(p);
      end;
      if p[0] = '}' then
        inc(p);
      commentLen := p - commentStart;
      AddSemanticToken(tokens, count, capacity, line, pos, commentLen, 13, 0); // 13 = comment
    end
    else
      inc(p);
  end;
end;

procedure QuickSortSemanticTokens(var tokens: TSemanticTokenArray; Low, High: integer);
var
  i, j: integer;
  pivot: TSemanticToken;
  temp: TSemanticToken;
begin
  if Low >= High then exit;
  pivot := tokens[Low + (High - Low) div 2];
  i := Low;
  j := High;
  repeat
    while (tokens[i].line < pivot.line) or ((tokens[i].line = pivot.line) and (tokens[i].position < pivot.position)) do
      inc(i);
    while (tokens[j].line > pivot.line) or ((tokens[j].line = pivot.line) and (tokens[j].position > pivot.position)) do
      dec(j);
    if i <= j then
    begin
      temp := tokens[i];
      tokens[i] := tokens[j];
      tokens[j] := temp;
      inc(i);
      dec(j);
    end;
  until i > j;
  QuickSortSemanticTokens(tokens, Low, j);
  QuickSortSemanticTokens(tokens, i, High);
end;

procedure HandleSemanticTokens(WriteStream: TStream; const Uri: string; Id: TJSONData);
var
  Response: string;
  tokens: TSemanticTokenArray;
  count, capacity: integer;
  i, k: integer;
  curToken: TToken;
  ident: TIdentifier;
  sym: TSymbol;
  tokenType, tokenModifiers: integer;
  isParam: boolean;
  parentsStack: array of string;
  parentsCount: integer;
  dataJson: string;
  prevLine, prevChar: integer;
  deltaLine, deltaChar: integer;
  tName: shortstring;
  identName: shortstring;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';
    
  count := 0;
  capacity := 256;
  SetLength(tokens, capacity);
  
  if (LastParserContext <> nil) and (LastParsedUri = Uri) then
  begin
    ScanComments(LastParserContext.GetContents, tokens, count, capacity);
    
    parentsCount := 0;
    SetLength(parentsStack, 64);
    
    for k := 0 to LastParserContext.tokensLen - 1 do
    begin
      curToken := LastParserContext.Tokens[k];
      if curToken = nil then continue;
      
      if curToken.state = tsEndOf then
      begin
        if parentsCount > 0 then
          dec(parentsCount);
        continue;
      end;
      
      if not curToken.isPrimitive then
      begin
        if parentsCount >= length(parentsStack) then
          SetLength(parentsStack, parentsCount + 64);
        parentsStack[parentsCount] := curToken.tokenName;
        inc(parentsCount);
        continue;
      end;
      
      if (curToken.len <= 0) or not (curToken.state in [tsCorrect, tsError, tsSkipped]) then
        continue;
        
      tokenType := -1;
      tokenModifiers := 0;
      tName := curToken.tokenName;
      
      if tName = 'RW' then
      begin
        if (TReservedWord(curToken).kind >= rwAnd) and (TReservedWord(curToken).kind <= rwTry) then
        begin
          tokenType := 10; // keyword
        end
        else if TReservedWord(curToken).kind in [
          rwAssign, rwPlus, rwMinus, rwMultiply, rwExponentiation, rwDivide, rwHat,
          rwEquals, rwNotEqual, rwLess, rwMore, rwLessOrEqual, rwMoreOrEqual,
          rwAt, rwShl2, rwShr2, rwSymmetricDifference
        ] then
        begin
          tokenType := 14; // operator
        end;
      end
      else if tName = 'Num' then
      begin
        tokenType := 11; // number
      end
      else if tName = 'Str' then
      begin
        tokenType := 12; // string
      end
      else if (tName = 'Ident') or (tName = 'SymbDecl') or (tName = 'SymbRef') then
      begin
        ident := TIdentifier(curToken);
        sym := TSymbol(ident.symbol);
        
        if sym <> nil then
        begin
          if sym.declaration = ident then
            tokenModifiers := tokenModifiers or 1; // declaration
            
          isParam := sym.isParameter;
          
          case sym.kind of
            skUnitName:
              tokenType := 0; // namespace
            skTypeName:
              begin
                if (sym.typeDef <> nil) and (sym.typeDef^.kind = tkClass) then
                  tokenType := 1 // class
                else if (sym.typeDef <> nil) and (sym.typeDef^.kind = tkRecord) then
                  tokenType := 3 // struct
                else
                  tokenType := 4; // type
              end;
            skConstant, skTypedConstant:
              begin
                tokenType := 6; // variable
                tokenModifiers := tokenModifiers or 4; // readonly
              end;
            skVariable:
              begin
                for i := parentsCount - 1 downto 0 do
                  if parentsStack[i] = 'ParameterDecl' then
                    isParam := true;
                    
                if isParam then
                  tokenType := 5 // parameter
                else if (sym.parent <> nil) and (sym.parent.kind = skTypeName) then
                  tokenType := 7 // property
                else
                  tokenType := 6; // variable
              end;
            skProcedure, skFunction:
              begin
                if (sym.parent <> nil) and (sym.parent.kind = skTypeName) then
                  tokenType := 9 // method
                else
                  tokenType := 8; // function
              end;
            skConstructor, skDestructor:
              tokenType := 9; // method
          end;
        end
        else
        begin
          identName := LowerCase(ident.GetStr());
          if (identName = 'public') or (identName = 'protected') or
             (identName = 'private') or (identName = 'published') or
             (identName = 'strict') then
            tokenType := 10 // keyword
          else if TypesList.Find(identName) <> nil then
            tokenType := 4; // type
        end;
      end;
      
      if tokenType <> -1 then
      begin
        AddSemanticToken(tokens, count, capacity, curToken.line, curToken.position, curToken.len, tokenType, tokenModifiers);
      end;
    end;
    
    SetLength(parentsStack, 0);
  end;
  
  if count > 0 then
  begin
    QuickSortSemanticTokens(tokens, 0, count - 1);
  end;
  
  dataJson := '';
  prevLine := 0;
  prevChar := 0;
  for i := 0 to count - 1 do
  begin
    if i > 0 then
      dataJson := dataJson + ',';
      
    deltaLine := tokens[i].line - prevLine;
    if deltaLine > 0 then
      deltaChar := tokens[i].position
    else
      deltaChar := tokens[i].position - prevChar;
      
    dataJson := dataJson + 
      IntToStr(deltaLine) + ',' +
      IntToStr(deltaChar) + ',' +
      IntToStr(tokens[i].len) + ',' +
      IntToStr(tokens[i].tokenType) + ',' +
      IntToStr(tokens[i].tokenModifiers);
      
    prevLine := tokens[i].line;
    prevChar := tokens[i].position;
  end;
  
  SetLength(tokens, 0);
  
  Response := Response + '"result":{"data":[' + dataJson + ']}}';
  SendResponse(WriteStream, Response);
end;

procedure ProcessRequest(WriteStream: TStream; const JsonStr: string);
var
  Json: TJSONData;
  Parser: TJSONParser;
  Method, Uri, Content, Response: string;
  Id: TJSONData;
begin
  Parser := TJSONParser.Create(JsonStr);
  try
    Json := Parser.Parse;
  except
    Parser.Free;
    exit;
  end;

  try
    if Json = nil then exit;
    
    if Json.FindPath('method') <> nil then
      Method := Json.FindPath('method').AsString
    else
      Method := '';
      
    Id := Json.FindPath('id');

    if Method = 'initialize' then
    begin
      Response := '{"jsonrpc":"2.0",';
      if Id <> nil then
        Response := Response + '"id":' + Id.AsJSON + ','
      else
        Response := Response + '"id":null,';
      Response := Response + '"result":{' +
        '"capabilities":{' +
          '"textDocumentSync":1,' +
          '"documentSymbolProvider":true,' +
          '"semanticTokensProvider":{' +
            '"legend":{' +
              '"tokenTypes":["namespace","class","interface","struct","type","parameter","variable","property","function","method","keyword","number","string","comment","operator"],' +
              '"tokenModifiers":["declaration","definition","readonly","static"]' +
            '},' +
            '"full":true' +
          '}' +
        '}' +
      '}}';
      SendResponse(WriteStream, Response);
    end
    else if Method = 'textDocument/didOpen' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      Content := Json.FindPath('params.textDocument.text').AsString;
      HandleFileChange(WriteStream, Uri, Content);
    end
    else if Method = 'textDocument/didChange' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      Content := Json.FindPath('params.contentChanges[0].text').AsString;
      HandleFileChange(WriteStream, Uri, Content);
    end
    else if Method = 'textDocument/documentSymbol' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      HandleDocumentSymbol(WriteStream, Uri, Id);
    end
    else if Method = 'textDocument/semanticTokens/full' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      HandleSemanticTokens(WriteStream, Uri, Id);
    end
    else if Method = 'shutdown' then
    begin
      Response := '{"jsonrpc":"2.0",';
      if Id <> nil then
        Response := Response + '"id":' + Id.AsJSON + ','
      else
        Response := Response + '"id":null,';
      Response := Response + '"result":null}';
      SendResponse(WriteStream, Response);
    end;
  finally
    Json.Free;
    Parser.Free;
  end;
end;

procedure ProcessStream(ReadStream, WriteStream: TStream);
var
  Line: string;
  ContentLength, BytesToRead: integer;
  JsonBody: string;
begin
  while true do
  begin
    ContentLength := 0;
    repeat
      if not ReadLine(ReadStream, Line) then
        exit;
      if Line = '' then
        break;
      if Copy(Line, 1, 15) = 'Content-Length:' then
      begin
        ContentLength := StrToIntDef(Trim(Copy(Line, 16, Length(Line) - 15)), 0);
      end;
    until false;
    
    if ContentLength > 0 then
    begin
      SetLength(JsonBody, ContentLength);
      BytesToRead := ContentLength;
      if ReadStream.Read(JsonBody[1], BytesToRead) <> BytesToRead then
        exit;
        
      ProcessRequest(WriteStream, JsonBody);
    end;
  end;
end;

procedure TLspServerApp.OnConnect(Sender: TObject; Data: TSocketStream);
begin
  if Data <> nil then
  begin
    try
      ProcessStream(Data, Data);
    except
      on E: Exception do
        WriteLn(StdErr, 'Error processing client stream: ', E.Message);
    end;
  end;
end;

procedure RunTcpServer(Port: integer);
var
  App: TLspServerApp;
  Server: TInetServer;
begin
  App := TLspServerApp.Create;
  Server := TInetServer.Create(Port);
  try
    Server.OnConnect := @App.OnConnect;
    WriteLn(StdErr, 'LSP Server listening on TCP port ', Port);
    Server.StartAccepting;
  finally
    Server.Free;
    App.Free;
  end;
end;

procedure RunStdioServer;
var
  InStream: THandleStream;
  OutStream: THandleStream;
begin
  WriteLn(StdErr, 'LSP Server starting in stdio mode');
  
  Close(output);
  Assign(output, 'stderr');
  Rewrite(output);
  
  InStream := THandleStream.Create(GetStdinHandle);
  OutStream := THandleStream.Create(GetStdoutHandle);
  try
    ProcessStream(InStream, OutStream);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

var
  Port: integer;
  UseStdio: boolean;
  I: integer;
  Param: string;
begin
  Port := 5007;
  UseStdio := false;
  for I := 1 to ParamCount do
  begin
    Param := ParamStr(I);
    if Param = '--stdio' then
      UseStdio := true
    else if Copy(Param, 1, 7) = '--port=' then
      Port := StrToIntDef(Copy(Param, 8, Length(Param) - 7), 5007);
  end;

  if UseStdio then
    RunStdioServer
  else
    RunTcpServer(Port);
end.
