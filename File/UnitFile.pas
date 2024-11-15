unit UnitFile;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TUnitFile = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;


implementation

uses
    Anchors, Symbols, TypeDefs, ReservedWord, Identifier,
    UsesClause, InterfaceBlock, Block;

constructor TUnitFile.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextIsComma: boolean;
    ident: TIdentifier;
    unitTypeDef: TTypeDef;
begin
    tokenName := 'UnitFile';
    ctx.parseUnit := Self;
    ctx.Add(Self);

    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwUnit, false);

    ident := TIdentifier.Create(ctx, false);
    // TODO: namespaced units
    unitTypeDef.kind := tkUnitName;
    RegisterSymbol(ident, nil, skUnitName, unitTypeDef, ctx.Cursor);

    TReservedWord.Create(ctx, rwSemiColon, false);

    TReservedWord.Create(ctx, rwInterface, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TInterfaceBlock.Create(ctx);

    TReservedWord.Create(ctx, rwImplementation, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TBlock.Create(ctx, [], unknownType, unknownType);

    TReservedWord.Create(ctx, rwDot, false);

    ctx.MarkEndOfToken(Self);
end;

end.
