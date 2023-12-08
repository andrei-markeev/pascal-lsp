unit UsesClause;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, ReservedWord, Identifier;

type
    TUsesClause = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TUsesClause.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'TUsesClause';
    ctx.Add(Self);
    start := ctx.Cursor;

    repeat
        TIdentifier.Create(ctx, false);
        // TODO: load and parse unit
        nextReservedWord := DetermineReservedWord(ctx);
        if nextReservedWord = rwComma then
            TReservedWord.Create(ctx, rwComma, true);
    until nextReservedWord <> rwComma;

    len := ctx.Cursor - start;
    ctx.MarkEndOfToken(Self);
end;

end.
