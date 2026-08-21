unit ParameterDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Parameters, Anchors, Token, TypedToken, Identifier;

type
    TParameterDecl = class(TTypedToken)
    public
        parameterKind: TParameterKind;
        idents: array of TIdentifier;
        hasDefaultValue: boolean;
        symbols: array of TSymbol;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    CompilationMode, TypeDefs, ReservedWord, TypeSpec, ConstValue, RecordTypeDef;

constructor TParameterDecl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i, l: integer;
    hasMoreMembers: boolean;
    symbolKind: TSymbolKind;
begin
    tokenName := 'ParameterDecl';
    ctx.Add(Self);

    hasDefaultValue := false;

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

    typeDef := unknownType;

    parameterKind := ptkValue;

    if nextTokenKind.reservedWordKind <> rwUnknown then
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        case nextTokenKind.reservedWordKind of
            rwConst: parameterKind := ptkConst;
            rwVar: parameterKind := ptkVar;
            rwOut: parameterKind := ptkOut;
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

    if parameterKind = ptkConst then
        symbolKind := skConstParameter
    else
        symbolKind := skParameter;

    SetLength(symbols, l);
    for i := 0 to l - 1 do
    begin
        symbols[i] := RegisterSymbol(idents[i], nil, symbolKind, typeDef, ctx.Cursor, false);
    end;

    if nextTokenKind.reservedWordKind = rwColon then
    begin
        TReservedWord.Create(ctx, rwColon, true);

        // TODO: open parameters (e.g. open arrays)
        // do we even need any special treatment?

        TTypeSpec.Create(ctx, symbols, typeDef);
        for i := 0 to l - 1 do
            symbols[i].typeDef := typeDef;

        CheckPartialRecordInstantiation(ctx, typeDef, Self);

        if PeekReservedWord(ctx, rwEquals) then
        begin
            hasDefaultValue := true;
            TReservedWord.Create(ctx, rwEquals, true);
            nextTokenKind := DetermineNextTokenKind(ctx);
            TConstValue.Create(ctx, nextTokenKind);
            if not (mfDefaultParamValues in Features[ctx.mode]) then
            begin
                state := tsError;
                errorMessage := 'Default parameter values are not supported in this compilation mode!';
            end;
        end;
    end
    else if parameterKind in [ptkConst, ptkVar, ptkOut] then
    begin
        if not (mfUntypedParams in Features[ctx.mode]) then
        begin
            state := tsError;
            errorMessage := 'Untyped parameters are not supported in this compilation mode!';
        end;
        // Untyped parameter: keep modifier (const/var/out) and unknownType
    end
    else
    begin
        state := tsError;
        errorMessage := 'Specify a type or provide a modifier (either ''const'' or ''var'') to create an untyped parameter!';
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
