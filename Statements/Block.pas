unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TBlock.Create(ctx: TParserContext);
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwBegin, false);
    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

destructor TBlock.Destroy;
begin
end;

end.
