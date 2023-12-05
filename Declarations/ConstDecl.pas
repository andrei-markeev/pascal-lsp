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
        destructor Destroy; override;
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
    ident := TIdentifier.Create(ctx);
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

    ctx.MarkEndOfToken(Self);

    if symbolKind = skConstant then
        typeDef.kind := value.valueType
    else
        typeDef := constType.typeDef;
    RegisterSymbol(ident, symbolKind, ctx.parseUnit, typeDef, ctx.Cursor);
end;

destructor TConstDecl.Destroy;
begin
    ident.Free;
    value.Free;
end;

end.
