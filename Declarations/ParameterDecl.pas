unit ParameterDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, Identifier, TypeSpec;

type
    TParameterDecl = class(TToken)
    public
        idents: array of TIdentifier;
        varType: TTypeSpec;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, TypeDefs, ReservedWord;

const
    unknownType: TTypeDef = (size: 1; kind: tkUnknown);

constructor TParameterDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i, l, p: integer;
    hasMoreMembers: boolean;
    symbols: array of TSymbol;
begin
    tokenName := 'ParameterDecl';
    ctx.Add(Self);

    start := ctx.Cursor;

    // TODO: constant parameters
    // TODO: variable parameters
    // TODO: untyped parameters
    // TODO: open parameters (e.g. open arrays)

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
        idents[l] := TIdentifier.Create(ctx, false);
        inc(l);
        ctx.SkipTrivia;
        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
           TReservedWord.Create(ctx, rwComma, true);
    until hasMoreMembers = false;

    AddAnchor(rwColon);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwColon);

    SetLength(symbols, l);
    for i := 0 to l - 1 do
        symbols[i] := RegisterSymbol(idents[i], nil, skVariable, unknownType, ctx.Cursor);

    TReservedWord.Create(ctx, rwColon, nextTokenKind.reservedWordKind = rwColon);
    varType := TTypeSpec.Create(ctx, symbols);

    for i := 0 to length(symbols) - 1 do
        symbols[i].typeDef := varType.typeDef;

    ctx.MarkEndOfToken(Self);
end;

end.
