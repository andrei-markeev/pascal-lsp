program LspServer;

{$mode objfpc}
{$longstrings on}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  sysutils, classes, ssockets, fpjson, jsonparser,
  ParserContext, Token, Symbols, Scopes, ReservedWord, TypeDecl,
  ParameterDecl, TypeDefs, ProgramFile, UnitFile;

type
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

function SerializeSymbol(symbol: TSymbol): string;
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
      if childJson <> '' then
        childJson := childJson + ',';
      childJson := childJson + SerializeSymbol(symbol.children[c]);
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
    '"name":' + '"' + string(StringToJSONString(symbol.shortName)) + '"' + ',' +
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
        if (symbol <> nil) and (symbol.declaration <> nil) and (symbol.parent = nil) then
        begin
          symbolSerialized := SerializeSymbol(symbol);
          
          if (interfaceBlock <> nil) and (symbol.declaration.start >= interfaceBlock.start) and (symbol.declaration.start < interfaceBlock.start + interfaceBlock.len) then
          begin
            if interfaceChildren <> '' then
              interfaceChildren := interfaceChildren + ',';
            interfaceChildren := interfaceChildren + symbolSerialized;
          end
          else if (implementationBlock <> nil) and (symbol.declaration.start >= implementationBlock.start) and (symbol.declaration.start < implementationBlock.start + implementationBlock.len) then
          begin
            if implementationChildren <> '' then
              implementationChildren := implementationChildren + ',';
            implementationChildren := implementationChildren + symbolSerialized;
          end
          else
          begin
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
          '"documentSymbolProvider":true' +
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
