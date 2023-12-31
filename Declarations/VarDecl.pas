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
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol);
    end;

implementation

const
    unknownType: TTypeDef = (size: 1; kind: tkUnknown);

constructor TVarDecl.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    nextTokenKind: TTokenKind;
    i, l, p: integer;
    hasMoreMembers: boolean;
    symbols: array of TSymbol;
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

    if (length(parentSymbols) > 0) and (parentSymbols[0] <> nil) then
        WriteLn('parentSymbol is not nil in VarDecl: ', parentSymbols[0].name);
    SetLength(symbols, l * length(parentSymbols));
    for p := 0 to length(parentSymbols) - 1 do
        for i := 0 to l - 1 do
            symbols[i + p * l] := RegisterSymbol(idents[i], parentSymbols[p], skVariable, ctx.parseUnit, unknownType, ctx.Cursor);

    TReservedWord.Create(ctx, rwColon, nextTokenKind.reservedWordKind = rwColon);
    varType := TTypeSpec.Create(ctx, symbols);

    for i := 0 to length(symbols) - 1 do
        symbols[i].typeDef := varType.typeDef;

    // TODO: variable initialization

    ctx.MarkEndOfToken(Self);
end;

end.
