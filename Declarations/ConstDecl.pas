unit ConstDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, TypedToken, Identifier, ConstValue;

type
    TConstDecl = class(TTypedToken)
    public
        ident: TIdentifier;
        constType: TTypeDef;
        value: TConstValue;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, Symbols, Token, ReservedWord, TypeSpec;

constructor TConstDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    symbolKind: TSymbolKind;
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
        CreateTypeSpec(ctx, typeDef);
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
        if not TypesAreAssignable(typeDef, value.typeDef, errorMessage) then
        begin
            state := tsError;
            errorMessage := 'Constant value cannot be assigned to the specified type: ' + errorMessage;
        end;
    end;

    ctx.MarkEndOfToken(Self);
    RegisterSymbol(ident, nil, symbolKind, @typeDef, ctx.Cursor);
end;

end.
