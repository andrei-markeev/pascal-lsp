unit Identifier;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TIdentifier = class(TToken)
    public
        symbol: pointer;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TIdentifier.Create(ctx: TParserContext);
begin
    tokenName := 'Ident';
    ctx.SkipTrivia;
    if ctx.Cursor[0] in ['a'..'z', 'A'..'Z', '_'] then
    begin
        start := ctx.Cursor;
        inc(ctx.Cursor);
        while ctx.Cursor[0] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
            inc(ctx.Cursor);
        state := tsCorrect;
        len := ctx.Cursor - start;
    end
    else
    begin
        start := ctx.Cursor;
        len := 0;
        state := tsMissing;
    end;

    ctx.Add(Self);
end;

destructor TIdentifier.Destroy;
begin
end;

end.
