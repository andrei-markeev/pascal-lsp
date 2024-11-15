unit TypeDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Token, Identifier;

type
    TTypeDecl = class(TToken)
    public
        ident: TIdentifier;
        declType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, Symbols, TypeSpec, ReservedWord;

constructor TTypeDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    symbol: TSymbol;
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

    declType := unknownType;

    symbol := RegisterSymbol(ident, nil, skTypeName, @declType, ctx.Cursor);

    AddAnchor(rwEquals);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwEquals);

    TReservedWord.Create(ctx, rwEquals, nextTokenKind.reservedWordKind = rwEquals);
    TTypeSpec.Create(ctx, [symbol], declType);

    ctx.MarkEndOfToken(Self);
end;

end.
