unit ConstDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, ConstValue, TypeSpec;

type
    TConstDecl = class(TToken)
    public
        ident: TIdentifier;
        constType: TTypeSpec;
        value: TConstValue;
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TConstDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    symbolKind: TSymbolKind;
    typeDef: TTypeDef;
begin
    tokenName := 'ConstDecl';
    ctx.Add(Self);

    start := ctx.Cursor;

    AddAnchor(pkIdentifier);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(pkIdentifier);

    if nextTokenKind.primitiveKind <> pkIdentifier then
    begin
        len := 0;
        state := tsMissing;
        exit;
    end;
    start := ctx.Cursor;
    ident := TIdentifier.Create(ctx, false);
    symbolKind := skConstant;

    AddAnchor(rwEquals);
    AddAnchor(rwColon);
    AddAnchor(pkNumber);
    AddAnchor(pkString);
    AddAnchor(pkIdentifier);
    nextTokenKind := SkipUntilAnchor(ctx);

    if nextTokenKind.reservedWordKind = rwColon then
    begin
        TReservedWord.Create(ctx, rwColon, true);
        constType := TTypeSpec.Create(ctx);
        symbolKind := skTypedConstant;
    end;
    RemoveAnchor(rwColon);

    TReservedWord.Create(ctx, rwEquals, false);
    RemoveAnchor(rwEquals);

    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);
    RemoveAnchor(pkIdentifier);

    value := TConstValue.Create(ctx, nextTokenKind);

    if symbolKind = skConstant then
        typeDef := value.typeDef
    else
    begin
        if not TypesAreAssignable(constType.typeDef, value.typeDef, errorMessage) then
        begin
            state := tsError;
            errorMessage := 'Constant value cannot be assigned to the specified type: ' + errorMessage;
        end;
        typeDef := constType.typeDef;
    end;

    ctx.MarkEndOfToken(Self);
    RegisterSymbol(ident, symbolKind, ctx.parseUnit, typeDef, ctx.Cursor);
end;

end.
