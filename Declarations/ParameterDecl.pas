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
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, ReservedWord, TypeSpec;

constructor TParameterDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i, l: integer;
    hasMoreMembers: boolean;
    isVar, isConst, isOut: boolean;
    symbolKind: TSymbolKind;
    symbols: array of TSymbol;
begin
    tokenName := 'ParameterDecl';
    ctx.Add(Self);

    start := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);
    if (nextTokenKind.primitiveKind <> pkIdentifier) and not (nextTokenKind.reservedWordKind in [rwConst, rwVar, rwOut]) then
    begin
        SetLength(idents, 0);
        len := 0;
        state := tsMissing;
        exit;
    end;

    start := ctx.Cursor;

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
        symbols[i] := RegisterSymbol(idents[i], nil, symbolKind, @typeDef, ctx.Cursor);

    if nextTokenKind.reservedWordKind = rwColon then
    begin
        TReservedWord.Create(ctx, rwColon, true);

        // TODO: open parameters (e.g. open arrays)
        // do we even need any special treatment?

        TTypeSpec.Create(ctx, symbols, typeDef);
    end
    else if isConst or isVar then
    begin
        // TODO: untyped parameters
    end
    else
    begin
        state := tsError;
        errorMessage := 'Specify a type or provide a modifier (either ''const'' or ''var'') to create an untyped parameter!';
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
