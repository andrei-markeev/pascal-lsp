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
  if LastFileToken <> nil then
  begin
    LastFileToken.Free;
    LastFileToken := nil;
  end;
  if LastParserContext <> nil then
  begin
    LastParserContext.Free;
    LastParserContext := nil;
  end;
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

procedure HandleDocumentSymbol(WriteStream: TStream; const Uri: string; Id: TJSONData);
var
  Response, SymbolJson: string;
  SymbolCount: integer;
  i, j: integer;
  scope: TScope;
  symbol: TSymbol;
  symbolKindVal: integer;
  symRange: string;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';
    
  SymbolJson := '';
  SymbolCount := 0;
  
  if (LastParserContext <> nil) and (LastParsedUri = Uri) then
  begin
    for i := 0 to length(ScopesList) - 1 do
    begin
      scope := ScopesList[i];
      for j := 0 to scope.symbolsList.Count - 1 do
      begin
        symbol := TSymbol(scope.symbolsList.Items[j]);
        if (symbol <> nil) and (symbol.declaration <> nil) then
        begin
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
          
          symRange := '{"start":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position) + '},' +
                      '"end":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position + symbol.declaration.len) + '}}';
          
          if SymbolCount > 0 then
            SymbolJson := SymbolJson + ',';
            
          SymbolJson := SymbolJson + '{' +
            '"name":' + '"' + string(StringToJSONString(symbol.name)) + '"' + ',' +
            '"kind":' + IntToStr(symbolKindVal) + ',' +
            '"range":' + symRange + ',' +
            '"selectionRange":' + symRange +
          '}';
          
          inc(SymbolCount);
        end;
      end;
    end;
  end;
  
  Response := Response + '"result":[' + SymbolJson + ']}';
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
