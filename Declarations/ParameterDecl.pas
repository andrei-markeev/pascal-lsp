unit ParameterDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Anchors, Token, TypedToken, Identifier;

type
    TParameterDecl = class(TTypedToken)
    public
        idents: array of TIdentifier;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind);
    end;

implementation

uses
    TypeDefs, ReservedWord, TypeSpec;

const
    unknownType: TTypeDef = (size: 1; kind: tkUnknown);

constructor TParameterDecl.Create(ctx: TParserContext; nextTokenKind: TTokenKind);
var
    i, l: integer;
    hasMoreMembers: boolean;
    isVar, isConst, isOut: boolean;
    symbolKind: TSymbolKind;
    symbols: array of TSymbol;
begin
    tokenName := 'ParameterDecl';
    ctx.Add(Self);

    start := ctx.Cursor;

    // TODO: constant parameters
    // TODO: variable parameters
    // TODO: untyped parameters
    // TODO: open parameters (e.g. open arrays)

    isConst := false;
    isVar := false;
    isOut := false;

    if nextTokenKind.reservedWordKind <> rwUnknown then
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        case nextTokenKind.reservedWordKind of
            rwConst: isConst := true;
            rwVar: isVar := true;
            rwOut: isOut := true;
        end;
    end;

    l := 0;
    repeat

        SetLength(idents, l + 1);
        idents[l] := TIdentifier.Create(ctx, false);
        inc(l);

        ctx.SkipTrivia;

        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
        begin
            TReservedWord.Create(ctx, rwComma, true);
            ctx.SkipTrivia;
        end;

    until hasMoreMembers = false;

    AddAnchor(rwColon);
    AddAnchor(rwSemiColon);
    AddAnchor(rwCloseParenthesis);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwColon);
    RemoveAnchor(rwSemiColon);
    RemoveAnchor(rwCloseParenthesis);

    symbolKind := skVariable;
    if isConst then
        if nextTokenKind.reservedWordKind = rwColon then
            symbolKind := skTypedConstant
        else
            symbolKind := skConstant;

    SetLength(symbols, l);
    for i := 0 to l - 1 do
        symbols[i] := RegisterSymbol(idents[i], nil, symbolKind, unknownType, ctx.Cursor);

    if nextTokenKind.reservedWordKind = rwColon then
    begin
        TReservedWord.Create(ctx, rwColon, true);
        typeDef := TTypeSpec.Create(ctx, symbols).typeDef;

        for i := 0 to length(symbols) - 1 do
            symbols[i].typeDef := typeDef;
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
