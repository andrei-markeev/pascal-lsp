unit RangeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, StringToken, Number;

type
    TRangeSpec = class(TToken)
    private
        procedure ParseBoundary(ctx: TParserContext; nextTokenKind: TTokenKind; var typeDefToFill: TTypeDef; var outToken: TToken);
    public
        fromToken: TToken;
        toToken: TToken;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind; var typeDefToFill: TTypeDef);
    end;

implementation

procedure TRangeSpec.ParseBoundary(ctx: TParserContext; nextTokenKind: TTokenKind; var typeDefToFill: TTypeDef; var outToken: TToken);
var
    ident: TIdentifier;
    symbol: TSymbol;
    typesAreCompatible: boolean;
    sign: TReservedWordKind;
begin
    sign := rwUnknown;
    if nextTokenKind.reservedWordKind in [rwMinus, rwPlus] then
    begin
        sign := nextTokenKind.reservedWordKind;
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    if nextTokenKind.primitiveKind = pkIdentifier then
    begin
        ident := TIdentifier.Create(ctx, true);
        outToken := ident;

        symbol := TSymbol(ident.symbol);
        if symbol.kind <> skConstant then
        begin
            state := tsError;
            errorMessage := 'Cannot use ' + ident.name + ' (' + SymbolKindStr[ord(symbol.kind)] + ') in a subrange declaration! Only constants are allowed.';
            exit;
        end;

        typesAreCompatible := typeDefToFill.kind = symbol.typeDef^.kind;
        if (typeDefToFill.kind = tkUnknown) then
            typesAreCompatible := true;
        if (typeDefToFill.kind = tkChar) and (symbol.typeDef^.kind = tkCharRange) then
            typesAreCompatible := true;
        if (typeDefToFill.kind = tkCharRange) and (symbol.typeDef^.kind = tkChar) then
            typesAreCompatible := true;
        if (typeDefToFill.kind = tkEnumMember) and (symbol.typeDef^.kind = tkEnumMember) then
            typesAreCompatible := typeDefToFill.enumSpec = symbol.typeDef^.enumSpec;
        if not typesAreCompatible then
        begin
            state := tsError;
            if typeDefToFill.kind = symbol.typeDef^.kind then
                errorMessage := 'Using values from different enums in the same subrange declaration is not supported!'
            else
                errorMessage := 'The type of ' + ident.name + ' (' + TypeKindStr[ord(symbol.typeDef^.kind)] + ') is not compatible with the inferred type of the subrange declaration (' + TypeKindStr[ord(typeDefToFill.kind)] + ')!';
            exit;
        end;

        if (sign <> rwUnknown) and not (symbol.typeDef^.kind in [tkInteger, tkReal]) then
        begin
            state := tsError;
            errorMessage := 'Sign ''' + ReservedWordStr[ord(sign)] + ''' cannot be applied to ' + ident.name + ' (' + TypeKindStr[ord(symbol.typeDef^.kind)] + ')!';
            exit;
        end;

        typeDefToFill := symbol.typeDef^;
    end
    else if nextTokenKind.primitiveKind = pkString then
    begin
        if not (typeDefToFill.kind in [tkUnknown, tkChar, tkCharRange]) then
        begin
            state := tsError;
            errorMessage := 'Expected ' + TypeKindStr[ord(typeDefToFill.kind)] + ' but found a character string!';
            exit;
        end;

        outToken := TStringToken.Create(ctx);

        if TStringToken(outToken).stringLen <> 1 then
        begin
            state := tsError;
            errorMessage := 'Strings cannot be used in subrange declarations. Only character strings are allowed, e.g. ''A'', #13, etc.';
            exit;
        end;

        if sign <> rwUnknown then
        begin
            state := tsError;
            errorMessage := 'Sign ''' + ReservedWordStr[ord(sign)] + ''' cannot be applied to a char!';
            exit;
        end;

        typeDefToFill.kind := tkCharRange;
        // TODO: charRangeStart, charRangeEnd

    end
    else if nextTokenKind.primitiveKind = pkNumber then
    begin
        typeDefToFill.kind := tkInteger;
        outToken := TNumber.Create(ctx);
    end;
end;

constructor TRangeSpec.Create(ctx: TParserContext; nextTokenKind: TTokenKind; var typeDefToFill: TTypeDef);
begin
    ctx.Add(Self);
    tokenName := 'RangeSpec';
    start := ctx.Cursor;
    state := tsCorrect;

    ParseBoundary(ctx, nextTokenKind, typeDefToFill, fromToken);

    TReservedWord.Create(ctx, rwRange, false);

    AddAnchor(pkIdentifier);
    AddAnchor(rwMinus);
    AddAnchor(rwPlus);
    AddAnchor(pkNumber);
    AddAnchor(pkString);

    nextTokenKind := SkipUntilAnchor(ctx);

    RemoveAnchor(pkIdentifier);
    RemoveAnchor(rwMinus);
    RemoveAnchor(rwPlus);
    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);

    ParseBoundary(ctx, nextTokenKind, typeDefToFill, toToken);

    ctx.MarkEndOfToken(Self);
end;

end.
