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
        varType: TTypeSpec;
        typeDef: PTypeDef;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
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
    ident := TIdentifier.Create(ctx);

    AddAnchor(rwEquals);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwEquals);

    TReservedWord.Create(ctx, rwEquals, nextTokenKind.reservedWordKind = rwEquals);
    varType := TTypeSpec.Create(ctx);

    ctx.MarkEndOfToken(Self);
    RegisterSymbol(ident, skTypeName, ctx.parseUnit, varType.typeDef, ctx.Cursor);
end;

destructor TTypeDecl.Destroy;
begin
    inherited;
end;

end.
