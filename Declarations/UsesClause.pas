unit UsesClause;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord, Identifier;

type
    TUsesClause = class(TToken)
    public
        units: array of TIdentifier;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TUsesClause.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
    l: integer;
begin
    tokenName := 'TUsesClause';
    ctx.Add(Self);
    start := ctx.Cursor;

    l := 0;
    repeat
        SetLength(units, l + 1);
        units[l] := TIdentifier.Create(ctx);
        inc(l);
        nextReservedWord := DetermineReservedWord(ctx);
        if nextReservedWord = rwComma then
            TReservedWord.Create(ctx, rwComma, true);
    until nextReservedWord <> rwComma;

    len := ctx.Cursor - start;
    ctx.MarkEndOfToken(Self);
end;

destructor TUsesClause.Destroy;
begin
end;

end.
