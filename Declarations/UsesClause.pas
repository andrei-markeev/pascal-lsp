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
        // TODO: load and parse unit
        nextReservedWord := DetermineReservedWord(ctx);
        if nextReservedWord = rwComma then
            TReservedWord.Create(ctx, rwComma, true);
    until nextReservedWord <> rwComma;

    TReservedWord.Create(ctx, rwSemiColon, false);

    len := ctx.Cursor - start;
    ctx.MarkEndOfToken(Self);
end;

end.
