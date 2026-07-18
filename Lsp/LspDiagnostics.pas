unit LspDiagnostics;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Token, ReservedWord, ProgramFile, UnitFile,
  LspUtils, LspState;

procedure HandleFileChange(WriteStream: TStream; const Uri: string; const Content: string);

implementation

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
  
  ctx := TParserContext.Create(UriToFilename(Uri), Content);
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

end.
