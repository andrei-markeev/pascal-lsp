unit RangeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, StringToken, Number;

type
    TRangeSpec = class(TToken)
    private
        procedure AddBoundaryAnchor;
        procedure RemoveBoundaryAnchor;
        procedure ParseBoundary(ctx: TParserContext; nextTokenKind: TTokenKind; var outToken: TToken);
    public
        fromToken: TToken;
        toToken: TToken;
        typeDef: TTypeDef;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind);
    end;

implementation

procedure TRangeSpec.AddBoundaryAnchor;
begin
    AddAnchor(pkIdentifier);
    case typeDef.kind of
        tkInteger:
            begin
                AddAnchor(rwMinus);
                AddAnchor(rwPlus);
                AddAnchor(pkNumber);
            end;
        tkChar, tkCharRange: AddAnchor(pkString);
    end;
end;

procedure TRangeSpec.RemoveBoundaryAnchor;
begin
    AddAnchor(pkIdentifier);
    case typeDef.kind of
        tkInteger:
            begin
                RemoveAnchor(rwMinus);
                RemoveAnchor(rwPlus);
                RemoveAnchor(pkNumber);
            end;
        tkChar, tkCharRange: RemoveAnchor(pkString);
    end;
end;

procedure TRangeSpec.ParseBoundary(ctx: TParserContext; nextTokenKind: TTokenKind; var outToken: TToken);
var
    identName: shortstring;
    symbol: TSymbol;
    typesAreCompatible: boolean;
begin
    if nextTokenKind.reservedWordKind in [rwMinus, rwPlus] then
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        AddAnchor(pkIdentifier);
        AddAnchor(pkNumber);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(pkIdentifier);
        RemoveAnchor(pkNumber);
        typeDef.kind := tkInteger;
        if nextTokenKind.reservedWordKind = rwMinus then
            typeDef.isSigned := true;
    end;

    if nextTokenKind.primitiveKind = pkIdentifier then
    begin
        outToken := TIdentifier.Create(ctx);
        identName := TIdentifier(outToken).GetName;
        symbol := FindSymbol(identName);
        if symbol = nil then
        begin
            state := tsError;
            errorMessage := 'Identifier "' + identName + '" has not been declared!';
            exit;
        end;

        if symbol.kind <> skConstant then
        begin
            state := tsError;
            errorMessage := 'Cannot use ' + identName + ' (' + SymbolKindStr[ord(symbol.kind)] + ') in a subrange declaration! Only constants are allowed.';
            exit;
        end;

        typesAreCompatible := typeDef.kind = symbol.typeDef.kind;
        if (typeDef.kind = tkUnknown) then
            typesAreCompatible := true;
        if (typeDef.kind = tkChar) and (symbol.typeDef.kind = tkCharRange) then
            typesAreCompatible := true;
        if (typeDef.kind = tkCharRange) and (symbol.typeDef.kind = tkChar) then
            typesAreCompatible := true;
        if (typeDef.kind = tkEnumMember) and (symbol.typeDef.kind = tkEnumMember) then
            typesAreCompatible := typeDef.enumSpec = symbol.typeDef.enumSpec;
        if not typesAreCompatible then
        begin
            state := tsError;
            errorMessage := 'The type of ' + identName + ' (' + TypeKindStr[ord(symbol.typeDef.kind)] + ') is not compatible with the inferred type of the subrange declaration (' + TypeKindStr[ord(typeDef.kind)] + ')!';
            exit;
        end;

        typeDef := symbol.typeDef;
    end
    else if nextTokenKind.primitiveKind = pkString then
    begin
        if not (typeDef.kind in [tkUnknown, tkChar, tkCharRange]) then
        begin
            state := tsError;
            errorMessage := 'Expected ' + TypeKindStr[ord(typeDef.kind)] + ' but found a character string!';
            exit;
        end;

        outToken := TStringToken.Create(ctx);

        if TStringToken(outToken).stringLen <> 1 then
        begin
            state := tsError;
            errorMessage := 'Strings cannot be used in subrange declarations. Only character strings are allowed, e.g. ''A'', #13, etc.';
            exit;
        end;

        typeDef.kind := tkCharRange;
        // TODO: charRangeStart, charRangeEnd

    end
    else if nextTokenKind.primitiveKind = pkNumber then
    begin
        typeDef.kind := tkInteger;
        outToken := TNumber.Create(ctx);
    end;
end;

constructor TRangeSpec.Create(ctx: TParserContext; nextTokenKind: TTokenKind);
begin
    ctx.Add(Self);
    tokenName := 'RangeSpec';
    start := ctx.Cursor;
    typeDef.kind := tkUnknown;
    state := tsCorrect;

    ParseBoundary(ctx, nextTokenKind, fromToken);

    TReservedWord.Create(ctx, rwRange, false);

    AddBoundaryAnchor;
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveBoundaryAnchor;

    ParseBoundary(ctx, nextTokenKind, toToken);

    ctx.MarkEndOfToken(Self);
end;

end.
