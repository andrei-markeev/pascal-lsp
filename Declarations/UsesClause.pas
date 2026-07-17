unit UsesClause;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, ReservedWord, Identifier, SystemUnits;

type
    TUsesClause = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    sysutils, UnitFile, LspConfig;

function ReadFileToString(const FileName: string): string;
var
    F: File;
    Len: Integer;
begin
    Result := '';
    if not FileExists(FileName) then
        Exit;
    Assign(F, FileName);
    Reset(F, 1);
    Len := FileSize(F);
    SetLength(Result, Len);
    if Len > 0 then
        BlockRead(F, Result[1], Len);
    Close(F);
end;

procedure LoadAndParseUnit(const UnitName: string; ctx: TParserContext);
var
    BaseDir, UnitPath, Content: string;
    UnitCtx: TParserContext;
    UnitFileToken: TToken;
    i: integer;
    Found: boolean;
    SearchPath: string;
begin
    if LoadedUnits.Find(LowerCase(UnitName)) <> nil then
        Exit;

    BaseDir := ExtractFilePath(UriToFilename(ctx.filePath));
    UnitPath := BaseDir + UnitName + '.pas';
    Found := FileExists(UnitPath);
    if not Found then
    begin
        UnitPath := BaseDir + LowerCase(UnitName) + '.pas';
        Found := FileExists(UnitPath);
    end;

    if not Found and (GConfig <> nil) then
    begin
        for i := 0 to GConfig.ResolvedSearchPaths.Count - 1 do
        begin
            SearchPath := IncludeTrailingPathDelimiter(GConfig.ResolvedSearchPaths[i]);
            UnitPath := SearchPath + UnitName + '.pas';
            Found := FileExists(UnitPath);
            if not Found then
            begin
                UnitPath := SearchPath + LowerCase(UnitName) + '.pas';
                Found := FileExists(UnitPath);
            end;
            if Found then
                Break;
        end;
    end;

    if not Found then
        Exit;

    Content := ReadFileToString(UnitPath);

    UnitCtx := TParserContext.Create(UnitPath, Content);
    UnitCtx.isDependency := True;

    LoadedUnits.Add(LowerCase(UnitName), UnitCtx);

    try
        UnitFileToken := TUnitFile.Create(UnitCtx);
    except
        on E: Exception do
            WriteLn('Error parsing unit ', UnitName, ': ', E.Message);
    end;
end;

constructor TUsesClause.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
    ident: TIdentifier;
begin
    tokenName := 'TUsesClause';
    ctx.Add(Self);
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwUses, true);

    repeat
        ident := TIdentifier.Create(ctx, false);
        LoadSystemUnit(ident.GetStr, ctx);
        LoadAndParseUnit(ident.GetStr, ctx);
        nextReservedWord := DetermineReservedWord(ctx);
        if nextReservedWord = rwComma then
            TReservedWord.Create(ctx, rwComma, true);
    until nextReservedWord <> rwComma;

    TReservedWord.Create(ctx, rwSemiColon, false);

    len := ctx.Cursor - start;
    ctx.MarkEndOfToken(Self);
end;

end.
