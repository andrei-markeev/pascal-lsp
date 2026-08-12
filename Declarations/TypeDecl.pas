unit TypeDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef, TypeDefs, Token, Identifier;

type
    TTypeDecl = class(TToken)
    public
        ident: TIdentifier;
        declType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, Symbols, TypeSpec, ReservedWord, RecordTypeDef;

constructor TTypeDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    symbol: TSymbol;
    existingSym: TSymbol;
begin
    tokenName := 'TypeDecl';
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

    existingSym := FindSymbol(ident.GetStr(), ctx.Cursor);
    if (existingSym <> nil) and (existingSym.kind = skTypeName) and (existingSym.typeDef <> nil) and (existingSym.typeDef.kind = tkRecord) and TRecordTypeDef(existingSym.typeDef).isPartial then
    begin
        symbol := existingSym;
        declType := existingSym.typeDef;
        existingSym.AddReference(ident);
    end
    else
    begin
        declType := unknownType;
        symbol := RegisterSymbol(ident, nil, skTypeName, declType, ctx.Cursor);
        symbol.rangeToken := Self;
    end;

    AddAnchor(rwEquals);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwEquals);

    TReservedWord.Create(ctx, rwEquals, nextTokenKind.reservedWordKind = rwEquals);
    TTypeSpec.Create(ctx, [symbol], declType);

    // Update registered symbol with final type
    symbol.typeDef := declType;
    if (declType <> nil) and (declType <> unknownType) and (declType.typeSymbol = nil) then
        declType.typeSymbol := symbol;

    ctx.MarkEndOfToken(Self);
end;

end.
