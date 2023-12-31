unit TypeDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, TypeSpec;

type
    TTypeDecl = class(TToken)
    public
        ident: TIdentifier;
        declType: TTypeSpec;
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TTypeDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
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

    AddAnchor(rwEquals);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwEquals);

    TReservedWord.Create(ctx, rwEquals, nextTokenKind.reservedWordKind = rwEquals);
    declType := CreateTypeSpec(ctx);

    ctx.MarkEndOfToken(Self);
    RegisterSymbol(ident, nil, skTypeName, ctx.parseUnit, declType.typeDef, ctx.Cursor);
end;

end.
