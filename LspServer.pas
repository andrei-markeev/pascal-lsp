program LspServer;

{$mode objfpc}
{$longstrings on}

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  sysutils, classes, ssockets, fpjson, jsonparser,
  ParserContext, Token, Identifier, Symbols, Scopes, ReservedWord, TypeDecl,
  ParameterDecl, TypeDefs, ProgramFile, UnitFile, LspConfig,
  LspUtils, LspState, LspDiagnostics, LspDocumentSymbols, LspSemanticTokens,
  LspDefinition, LspCompletion;

type
  TLspServerApp = class
  public
    procedure OnConnect(Sender: TObject; Data: TSocketStream);
  end;

procedure ProcessRequest(WriteStream: TStream; const JsonStr: string);
var
  Json: TJSONData;
  Parser: TJSONParser;
  Method, Uri, Content, Response: string;
  Id: TJSONData;
  RootUriData, RootPathData, InitOptions, OptLpi, OptDproj, OptScan, OptPaths, OptMaxCache: TJSONData;
  WorkspaceRoot: string;
  Idx: integer;
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
      RootUriData := Json.FindPath('params.rootUri');
      if (RootUriData <> nil) and (RootUriData.JSONType = jtString) then
        WorkspaceRoot := UriToFilename(RootUriData.AsString)
      else
      begin
        RootPathData := Json.FindPath('params.rootPath');
        if (RootPathData <> nil) and (RootPathData.JSONType = jtString) then
          WorkspaceRoot := RootPathData.AsString
        else
          WorkspaceRoot := '';
      end;

      InitOptions := Json.FindPath('params.initializationOptions');
      if InitOptions <> nil then
      begin
        OptLpi := InitOptions.FindPath('readLpi');
        if OptLpi <> nil then
          GConfig.ReadLpi := OptLpi.AsBoolean;

        OptDproj := InitOptions.FindPath('readDproj');
        if OptDproj <> nil then
          GConfig.ReadDproj := OptDproj.AsBoolean;

        OptScan := InitOptions.FindPath('scanProjectFolders');
        if OptScan <> nil then
          GConfig.ScanProjectFolders := OptScan.AsBoolean;

        OptMaxCache := InitOptions.FindPath('maxDocumentCacheSize');
        if OptMaxCache <> nil then
          GConfig.MaxDocumentCacheSize := OptMaxCache.AsInteger;

        OptPaths := InitOptions.FindPath('configuredPaths');
        if (OptPaths <> nil) and (OptPaths.JSONType = jtArray) then
        begin
          GConfig.UseConfiguredPaths := true;
          GConfig.ConfiguredPaths.Clear;
          for Idx := 0 to OptPaths.Count - 1 do
            GConfig.ConfiguredPaths.Add(OptPaths.Items[Idx].AsString);
        end;
      end;

      if WorkspaceRoot <> '' then
      begin
        GConfig.SetWorkspaceRoot(WorkspaceRoot);
        GConfig.ResolveSearchPaths;
      end;

      Response := '{"jsonrpc":"2.0",';
      if Id <> nil then
        Response := Response + '"id":' + Id.AsJSON + ','
      else
        Response := Response + '"id":null,';
      Response := Response + '"result":{' +
        '"capabilities":{' +
          '"textDocumentSync":1,' +
          '"completionProvider":{' +
            '"triggerCharacters":["."]' +
          '},' +
          '"documentSymbolProvider":true,' +
          '"definitionProvider":true,' +
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
    else if Method = 'textDocument/didClose' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      HandleFileClose(Uri);
    end
    else if Method = 'textDocument/documentSymbol' then
    begin
      Uri := Json.FindPath('params.textDocument.uri').AsString;
      HandleDocumentSymbol(WriteStream, Uri, Id);
    end
    else if Method = 'textDocument/definition' then
    begin
      HandleDefinition(WriteStream, Id, Json.FindPath('params'));
    end
    else if Method = 'textDocument/completion' then
    begin
      HandleCompletion(WriteStream, Id, Json.FindPath('params'));
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
  
  output := StdErr;
  
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
