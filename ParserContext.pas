unit ParserContext;

{$mode objfpc}
{$longstrings on}

interface

uses
    classes, strings, contnrs, Token, CompilationMode, TypeDefs;

type
    TParserContext = class(TTypeDefTracker)
    private
        triviaSkippedUntil: PChar;
        tokensCapacity: integer;
        contents: string;
        line: integer;
        lineStart: PChar;
    public
        cursorBeforeTrivia: PChar;
        Tokens: array of TToken;
        tokensLen: integer;
        typeDefs: array of TTypeDef;
        typeDefsLen: integer;
        Cursor: PChar;
        parseUnit: TToken;
        mode: TCompilationMode;
        filePath: string;
        isDependency: boolean;
        constructor Create(AFilePath: string; AFileContents: string);
        destructor Destroy; override;
        procedure TrackTypeDef(typeDef: TObject); override;
        function IsSeparator(ch: char): boolean; inline;
        function IsEOF: boolean; inline;
        function GetCursorBeforeTrivia: PChar; inline;
        function GetContents: string; inline;
        procedure SkipTrivia;
        procedure Add(token: TToken);
        procedure InsertBefore(refToken, tokenToInsert: TToken);
        procedure MarkEndOfToken(token: TToken);
    end;

var
    LoadedUnits: TFPHashList;
    ActiveContexts: TFPList;
    LastFoundContext: TParserContext;

procedure ClearLoadedUnits;
function UriToFilename(const Uri: string): string;
function FindContextForCursor(cursor: PChar): TParserContext;


implementation

uses
    sysutils, SystemUnits;

constructor TParserContext.Create(AFilePath: string; AFileContents: string);
var
    typeDefsCapacity: integer;
begin
    filePath := AFilePath;
    contents := AFileContents;
    isDependency := false;
    Cursor := PChar(contents);
    triviaSkippedUntil := nil;
    tokensCapacity := 1 + length(contents) div 10;
    SetLength(Tokens, tokensCapacity);
    typeDefsLen := 0;
    typeDefsCapacity := 1 + length(contents) div 200;
    if typeDefsCapacity < 4 then typeDefsCapacity := 4;
    if typeDefsCapacity > 128 then typeDefsCapacity := 128;
    SetLength(typeDefs, typeDefsCapacity);
    line := 0;
    lineStart := Cursor;
    mode := cmFreePascal;
    InitPredefinedTypes(mode);
    RegisterSystemSymbols(Self);
    if ActiveContexts <> nil then
        ActiveContexts.Add(Self);
end;

procedure TParserContext.TrackTypeDef(typeDef: TObject);
begin
    if typeDef = nil then exit;
    if typeDefsLen = length(typeDefs) then
        SetLength(typeDefs, typeDefsLen + 16);
    typeDefs[typeDefsLen] := TTypeDef(typeDef);
    inc(typeDefsLen);
end;

destructor TParserContext.Destroy;
var i: integer;
begin
    if LastFoundContext = Self then
        LastFoundContext := nil;
    if ActiveContexts <> nil then
        ActiveContexts.Remove(Self);
    for i := 0 to length(Tokens) - 1 do
        Tokens[i].Free;
    SetLength(Tokens, 0);
    for i := 0 to typeDefsLen - 1 do
        typeDefs[i].Free;
    SetLength(typeDefs, 0);
end;

function TParserContext.GetCursorBeforeTrivia: PChar; inline;
begin
    if triviaSkippedUntil = Cursor then
        GetCursorBeforeTrivia := cursorBeforeTrivia
    else
        GetCursorBeforeTrivia := Cursor;
end;

procedure TParserContext.SkipTrivia;
var
    before: PChar;
begin
    if triviaSkippedUntil = Cursor then
        exit;
    before := Cursor;
    while Cursor[0] in [#9, #10, #13, ' ', '{', '/'] do
    begin
        if (Cursor[0] = '/') and (Cursor[1] = '/') then
            repeat
                inc(Cursor);
            until Cursor[0] in [#10, #13, #0]
        else if Cursor[0] = '{' then
        begin
            if (Cursor[1] = '$') and (strlicomp(Cursor, PChar('{$mode'), 6) = 0) then
            begin
                inc(Cursor, 6);
                if Cursor[0] in [#9, ' '] then
                begin
                    while Cursor[0] in [#9, ' '] do
                        inc(Cursor);

                    if (strlicomp(Cursor, PChar('iso'), 3) = 0) and (Cursor[3] in [' ',#9,'}']) then
                        mode := cmStandardPascal
                    else if (strlicomp(Cursor, PChar('extpas'), 6) = 0) and (Cursor[6] in [' ',#9,'}']) then
                        mode := cmExtendedPascal
                    else if (strlicomp(Cursor, PChar('tp'), 2) = 0) and (Cursor[2] in [' ',#9,'}']) then
                        mode := cmTurboPascal
                    else if (strlicomp(Cursor, PChar('macpas'), 6) = 0) and (Cursor[6] in [' ',#9,'}']) then
                        mode := cmMacPascal
                    else if (strlicomp(Cursor, PChar('fpc'), 3) = 0) and (Cursor[3] in [' ',#9,'}']) then
                        mode := cmFreePascal
                    else if (strlicomp(Cursor, PChar('objfpc'), 6) = 0) and (Cursor[6] in [' ',#9,'}']) then
                        mode := cmObjectFreePascal
                    else if (strlicomp(Cursor, PChar('delphi'), 6) = 0) and (Cursor[6] in [' ',#9,'}']) then
                        mode := cmDelphi;

                    InitPredefinedTypes(mode);
                end;
            end;
            repeat
                inc(Cursor);
            until Cursor[0] in ['}', #0];
            if Cursor[0] = '}' then
                inc(Cursor);
        end
        else
        if (Cursor[0] = #13) and (Cursor[1] = #10) then
        begin
            inc(Cursor, 2);
            inc(line);
            lineStart := Cursor;
        end
        else if Cursor[0] in [#10, #13] then
        begin
            inc(Cursor);
            inc(line);
            lineStart := Cursor;
        end
        else
            inc(Cursor);
    end;
    triviaSkippedUntil := Cursor;
    cursorBeforeTrivia := before;
end;

function TParserContext.IsSeparator(ch: char): boolean; inline;
begin
    IsSeparator := not (ch in ['a'..'z', 'A'..'Z', '_', '0'..'9']);
end;

procedure TParserContext.Add(token: TToken);
begin
    if tokensLen = tokensCapacity then
    begin
        inc(tokensCapacity, 10);
        SetLength(Tokens, tokensCapacity)
    end;

    Tokens[tokensLen] := token;
    inc(tokensLen);

    token.line := line;
    if (token.start <> nil) and (token.start >= lineStart) then
        token.position := token.start - lineStart
    else if Cursor >= lineStart then
        token.position := Cursor - lineStart
    else
        token.position := 0;
end;

procedure TParserContext.InsertBefore(refToken, tokenToInsert: TToken);
var
    atIndex: integer;
    i: integer;
begin
    if tokensLen = tokensCapacity then
    begin
        inc(tokensCapacity, 10);
        SetLength(Tokens, tokensCapacity)
    end;

    tokenToInsert.line := refToken.line;
    tokenToInsert.position := refToken.position;

    atIndex := -1;
    for i := tokensLen downto 0 do
        if Tokens[i] = refToken then
        begin
            atIndex := i;
            break
        end;

    for i := tokensLen downto atIndex + 1 do
        Tokens[i] := Tokens[i - 1];

    Tokens[atIndex] := tokenToInsert;
    inc(tokensLen);
end;

procedure TParserContext.MarkEndOfToken(token: TToken);
var
    endOf: TToken;
    endCursor: PChar;
begin
    if triviaSkippedUntil = Cursor then
        endCursor := cursorBeforeTrivia
    else
        endCursor := Cursor;

    token.len := endCursor - token.start;
    endOf := TToken.Create;
    endOf.tokenName := token.tokenName;
    endOf.start := endCursor;
    endOf.len := 0;
    endOf.state := tsEndOf;
    token.endMarker := endOf;
    Add(endOf);
end;

function TParserContext.IsEOF: boolean; inline;
begin
    IsEOF := Cursor[0] = #0;
end;

function TParserContext.GetContents: string; inline;
begin
    GetContents := contents;
end;

procedure ClearLoadedUnits;
var
    i: integer;
    ctx: TParserContext;
begin
    if LoadedUnits <> nil then
    begin
        for i := 0 to LoadedUnits.Count - 1 do
        begin
            ctx := TParserContext(LoadedUnits.Items[i]);
            if (ctx <> nil) and ctx.isDependency then
                ctx.Free;
        end;
        LoadedUnits.Clear;
    end;
end;

function UriToFilename(const Uri: string): string;
begin
    if Copy(Uri, 1, 8) = 'file:///' then
        Result := Copy(Uri, 9, Length(Uri) - 8)
    else
        Result := Uri;
    Result := StringReplace(Result, '%20', ' ', [rfReplaceAll]);
    Result := StringReplace(Result, '%3A', ':', [rfReplaceAll]);
    Result := StringReplace(Result, '%3a', ':', [rfReplaceAll]);
end;

function FindContextForCursor(cursor: PChar): TParserContext;
var
    i: integer;
    ctx: TParserContext;
begin
    if (LastFoundContext <> nil) and 
       (cursor >= PChar(LastFoundContext.contents)) and 
       (cursor <= PChar(LastFoundContext.contents) + Length(LastFoundContext.contents)) then
    begin
        Result := LastFoundContext;
        exit;
    end;

    Result := nil;
    if ActiveContexts = nil then
        exit;
    for i := 0 to ActiveContexts.Count - 1 do
    begin
        ctx := TParserContext(ActiveContexts.Items[i]);
        if (cursor >= PChar(ctx.contents)) and (cursor <= PChar(ctx.contents) + Length(ctx.contents)) then
        begin
            LastFoundContext := ctx;
            Result := ctx;
            exit;
        end;
    end;
end;

initialization
    LoadedUnits := TFPHashList.Create;
    ActiveContexts := TFPList.Create;
    LastFoundContext := nil;
finalization
    ClearLoadedUnits;
    LoadedUnits.Free;
    ActiveContexts.Free;

end.
