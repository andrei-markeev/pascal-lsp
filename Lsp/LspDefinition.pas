unit LspDefinition;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Token, Identifier, Symbols,
  LspUtils, LspState;

procedure HandleDefinition(WriteStream: TStream; Id: TJSONData; Params: TJSONData);

implementation

procedure HandleDefinition(WriteStream: TStream; Id: TJSONData; Params: TJSONData);
var
  Uri, DeclUri, ResultJson, Response: string;
  TargetLine, TargetCharacter: integer;
  TargetIdent, DeclIdent: TIdentifier;
  i: integer;
  curToken: TToken;
  Sym: TSymbol;
  DeclCtx: TParserContext;
  UnitName: string;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';

  ResultJson := 'null';

  if (Params <> nil) and (LastParserContext <> nil) then
  begin
    Uri := Params.FindPath('textDocument.uri').AsString;
    TargetLine := Params.FindPath('position.line').AsInteger;
    TargetCharacter := Params.FindPath('position.character').AsInteger;

    if LastParsedUri = Uri then
    begin
      TargetIdent := nil;
      for i := 0 to LastParserContext.tokensLen - 1 do
      begin
        curToken := LastParserContext.Tokens[i];
        if (curToken <> nil) and (curToken is TIdentifier) then
        begin
          if (curToken.line = TargetLine) and
             (TargetCharacter >= curToken.position) and
             (TargetCharacter <= curToken.position + curToken.len) then
          begin
            TargetIdent := TIdentifier(curToken);
            Break;
          end;
        end;
      end;

      if TargetIdent <> nil then
      begin
        if TargetIdent.symbol <> nil then
        begin
          Sym := TSymbol(TargetIdent.symbol);
          if Sym.declaration <> nil then
          begin
            DeclIdent := Sym.declaration;
            DeclCtx := FindContextForCursor(DeclIdent.start);
            if DeclCtx <> nil then
            begin
              DeclUri := FilenameToUri(DeclCtx.filePath);
              ResultJson := '{' +
                '"uri":"' + string(StringToJSONString(DeclUri)) + '",' +
                '"range":{' +
                  '"start":{"line":' + IntToStr(DeclIdent.line) + ',"character":' + IntToStr(DeclIdent.position) + '},' +
                  '"end":{"line":' + IntToStr(DeclIdent.line) + ',"character":' + IntToStr(DeclIdent.position + DeclIdent.len) + '}' +
                '}' +
              '}';
            end;
          end;
        end
        else
        begin
          UnitName := LowerCase(TargetIdent.GetStr());
          DeclCtx := TParserContext(LoadedUnits.Find(UnitName));
          if DeclCtx <> nil then
          begin
            DeclUri := FilenameToUri(DeclCtx.filePath);
            ResultJson := '{' +
              '"uri":"' + string(StringToJSONString(DeclUri)) + '",' +
              '"range":{' +
                '"start":{"line":0,"character":0},' +
                '"end":{"line":0,"character":0}' +
              '}' +
            '}';
          end;
        end;
      end;
    end;
  end;

  Response := Response + '"result":' + ResultJson + '}';
  SendResponse(WriteStream, Response);
end;

end.
