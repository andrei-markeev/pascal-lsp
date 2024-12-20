unit ParserContext;

{$mode objfpc}
{$longstrings on}

interface

uses
    strings, Token, CompilationMode, TypeDefs;

type
    TParserContext = class
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
        Cursor: PChar;
        parseUnit: TToken;
        mode: TCompilationMode;
        constructor Create(fileContents: string);
        destructor Destroy; override;
        function IsSeparator(ch: char): boolean; inline;
        function IsEOF: boolean; inline;
        function GetCursorBeforeTrivia: PChar; inline;
        procedure SkipTrivia;
        procedure Add(token: TToken);
        procedure InsertBefore(refToken, tokenToInsert: TToken);
        procedure MarkEndOfToken(token: TToken);
    end;

implementation

uses
    SystemUnits;

constructor TParserContext.Create(fileContents: string);
begin
    contents := fileContents;
    Cursor := PChar(contents);
    triviaSkippedUntil := nil;
    tokensCapacity := 1 + length(contents) div 10;
    SetLength(Tokens, tokensCapacity);
    line := 0;
    mode := cmFreePascal;
    InitPredefinedTypes(mode);
    RegisterSystemSymbols(Self);
end;

destructor TParserContext.Destroy;
var i: integer;
begin
    for i := 0 to length(Tokens) - 1 do
        Tokens[i].Free;
    SetLength(Tokens, 0);
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
                    else if (strlicomp(Cursor, PChar('tp'), 2) = 0) and (Cursor[2] in [' ',#9,'}']) then
                        mode := cmTurboPascal
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
    token.position := Cursor - lineStart;
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

end.
