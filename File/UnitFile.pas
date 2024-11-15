unit UnitFile;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TUnitFile = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
    end;


implementation

uses
    Symbols, TypeDefs, ReservedWord, Identifier,
    UsesClause, InterfaceBlock, ImplementationBlock;

constructor TUnitFile.Create(ctx: TParserContext);
var
    ident: TIdentifier;
begin
    tokenName := 'UnitFile';
    ctx.parseUnit := Self;
    ctx.Add(Self);

    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwUnit, false);

    ident := TIdentifier.Create(ctx, false);
    // TODO: namespaced units
    typeDef.kind := tkUnitName;
    RegisterSymbol(ident, nil, skUnitName, @typeDef, ctx.Cursor);

    TReservedWord.Create(ctx, rwSemiColon, false);

    TReservedWord.Create(ctx, rwInterface, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TInterfaceBlock.Create(ctx);

    TReservedWord.Create(ctx, rwImplementation, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TImplementationBlock.Create(ctx);

    TReservedWord.Create(ctx, rwDot, false);

    ctx.MarkEndOfToken(Self);
end;

end.
