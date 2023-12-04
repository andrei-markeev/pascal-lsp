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
    isPrimitive := true;
    start := ctx.Cursor;
    repeat
        inc(ctx.Cursor);
    until ctx.Cursor[0] in [
        '0'..'9', '_', '''', '#', 'A'..'Z', 'a'..'z',
        '+', '-', '*', '/', '^', '=', '<', '>', '(', ')', '[', ']', '{', '}',
        '.', ',', ':', ';', #0
    ];
    len := ctx.Cursor - start;
    state := tsSkipped;
    ctx.Add(Self);
end;

end.