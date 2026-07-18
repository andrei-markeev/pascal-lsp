unit LspState;

{$mode objfpc}
{$longstrings on}

interface

uses
  ParserContext, Token, TypeDefs, Scopes;

var
  LastParsedUri: string = '';
  LastParserContext: TParserContext = nil;
  LastFileToken: TToken = nil;

procedure FreeLastParsed;

implementation

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
  ClearLoadedUnits;
end;

end.
