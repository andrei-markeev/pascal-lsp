unit InvalidSymbol;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TInvalidSymbol = class(TToken)
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TInvalidSymbol.Create(ctx: TParserContext);
begin
    tokenName := 'Invalid';
    ctx.Add(Self);
    start := ctx.Cursor;
    if ctx.Cursor[0] = #0 then
    begin
        len := 0;
        state := tsError;
        errorMessage := 'Unexpected end of file!';
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    repeat
        inc(ctx.Cursor);
    until ctx.Cursor[0] in [
        '0'..'9', '_', '''', '#', 'A'..'Z', 'a'..'z',
        '+', '-', '*', '/', '^', '=', '<', '>', '(', ')', '[', ']', '{', '}',
        '.', ',', ':', ';', #0
    ];
    state := tsSkipped;
    ctx.MarkEndOfToken(Self);
end;

end.
