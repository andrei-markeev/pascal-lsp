unit VarDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, TypeSpec;

type
    TVarDecl = class(TToken)
    public
        ident: TIdentifier;
        varType: TTypeSpec;
        typeDef: PTypeDef;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TVarDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
begin
    tokenName := 'VarDecl';
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

    AddAnchor(rwColon);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwColon);

    TReservedWord.Create(ctx, rwColon, nextTokenKind.reservedWordKind = rwColon);
    varType := TTypeSpec.Create(ctx);

    // todo: variable initialization

    ctx.MarkEndOfToken(Self);
    RegisterSymbol(ident, skVariable, ctx.parseUnit, varType.typeDef, ctx.Cursor);
end;

destructor TVarDecl.Destroy;
begin
    inherited;
end;

end.
