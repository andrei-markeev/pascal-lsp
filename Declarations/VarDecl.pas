unit VarDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, TypeSpec;

type
    TVarDecl = class(TToken)
    public
        idents: array of TIdentifier;
        varType: TTypeSpec;
        typeDef: PTypeDef;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TVarDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i, l: integer;
    hasMoreMembers: boolean;
begin
    tokenName := 'VarDecl';
    ctx.Add(Self);

    start := ctx.Cursor;

    AddAnchor(pkIdentifier);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(pkIdentifier);

    if nextTokenKind.primitiveKind <> pkIdentifier then
    begin
        SetLength(idents, 0);
        len := 0;
        state := tsMissing;
        exit;
    end;
    start := ctx.Cursor;
    l := 0;
    repeat
        SetLength(idents, l + 1);
        idents[l] := TIdentifier.Create(ctx);
        inc(l);
        ctx.SkipTrivia;
        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
           TReservedWord.Create(ctx, rwComma, true);
    until hasMoreMembers = false;

    AddAnchor(rwColon);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwColon);

    TReservedWord.Create(ctx, rwColon, nextTokenKind.reservedWordKind = rwColon);
    varType := TTypeSpec.Create(ctx);

    // TODO: variable initialization

    ctx.MarkEndOfToken(Self);
    for i := 0 to l - 1 do
        RegisterSymbol(idents[i], skVariable, ctx.parseUnit, varType.typeDef, ctx.Cursor);
end;

destructor TVarDecl.Destroy;
begin
    inherited;
end;

end.
