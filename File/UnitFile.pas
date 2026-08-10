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
    Token, CompilationMode, Symbols, TypeDefs, TypeDef, PrimitiveTypeDef, ReservedWord, Identifier,
    UsesClause, InterfaceBlock, ImplementationBlock;

constructor TUnitFile.Create(ctx: TParserContext);
var
    ident, segIdent: TIdentifier;
    dotToken: TReservedWord;
    unitNameStr: string;
    parentSym, childSym, unitSym: TSymbol;
    unitTypeDef: TTypeDef;
begin
    tokenName := 'UnitFile';
    ctx.parseUnit := Self;
    ctx.Add(Self);

    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwUnit, false);

    ident := TIdentifier.Create(ctx, false);
    unitNameStr := ident.GetStr();
    unitTypeDef := TPrimitiveTypeDef.Create(ctx, tkUnitName);

    unitSym := FindSymbol(unitNameStr, ctx.Cursor);
    if unitSym = nil then
        unitSym := RegisterSymbol(ident, nil, skUnitName, unitTypeDef, ctx.Cursor);
    parentSym := unitSym;

    while PeekReservedWord(ctx, rwDot) do
    begin
        dotToken := TReservedWord.Create(ctx, rwDot, true);
        if not (mfNamespacedUnits in Features[ctx.mode]) then
        begin
            dotToken.state := tsError;
            dotToken.errorMessage := 'Namespaced units are not supported in this compilation mode!';
        end;
        segIdent := TIdentifier.Create(ctx, false);
        unitNameStr := unitNameStr + '.' + segIdent.GetStr();
        childSym := FindSymbol(parentSym, segIdent.GetStr(), ctx.Cursor);
        if childSym = nil then
            childSym := RegisterSymbol(segIdent, parentSym, skUnitName, unitTypeDef, ctx.Cursor);
        parentSym := childSym;
        unitSym := childSym;
    end;

    typeDef := unitTypeDef;

    if LoadedUnits.Find(LowerCase(unitNameStr)) = nil then
        LoadedUnits.Add(LowerCase(unitNameStr), ctx);

    TReservedWord.Create(ctx, rwSemiColon, false);

    TReservedWord.Create(ctx, rwInterface, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TInterfaceBlock.Create(ctx);

    TReservedWord.Create(ctx, rwImplementation, false);

    if ctx.isDependency then
    begin
        ctx.Cursor := ctx.Cursor + strlen(ctx.Cursor);
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TImplementationBlock.Create(ctx);

    TReservedWord.Create(ctx, rwDot, false);

    ctx.MarkEndOfToken(Self);
end;

end.
