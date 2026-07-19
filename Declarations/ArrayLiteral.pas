unit ArrayLiteral;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken, TypeDefs;

type
    TArrayLiteral = class(TTypedToken)
    public
        elements: array of TTypedToken;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    CompilationMode, Anchors, Token, ReservedWord, ConstValue, ArrayTypeDef;

constructor TArrayLiteral.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    elemValue: TConstValue;
    elemType: TTypeDef;
    elemCount: integer;
    nextRW: TReservedWordKind;
begin
    ctx.Add(Self);
    tokenName := 'ArrayLiteral';
    start := ctx.Cursor;
    typeDef := unknownType;
    state := tsCorrect;

    if ctx.mode < cmTurboPascal then
    begin
        start := ctx.cursorBeforeTrivia;
        state := tsMissing;
        len := 0;
        exit;
    end;

    TReservedWord.Create(ctx, rwOpenParenthesis, true);

    elemType := nil;
    elemCount := 0;

    AddAnchor(rwComma);
    AddAnchor(rwCloseParenthesis);
    AddAnchor(pkNumber);
    AddAnchor(pkString);
    AddAnchor(pkIdentifier);
    AddAnchor(rwOpenParenthesis);

    nextTokenKind := SkipUntilAnchor(ctx);

    while (nextTokenKind.reservedWordKind <> rwCloseParenthesis) and (nextTokenKind.primitiveKind <> pkUnknown) do
    begin
        elemValue := TConstValue.Create(ctx, nextTokenKind);
        inc(elemCount);
        SetLength(elements, elemCount);
        elements[elemCount - 1] := elemValue;

        if (elemValue <> nil) and (elemValue.typeDef <> nil) and (elemValue.typeDef <> unknownType) then
        begin
            if elemType = nil then
                elemType := elemValue.typeDef
            else if elemType.kind <> elemValue.typeDef.kind then
                elemType := unknownType;
        end;

        nextTokenKind := SkipUntilAnchor(ctx);
        if nextTokenKind.reservedWordKind = rwComma then
        begin
            TReservedWord.Create(ctx, rwComma, true);
            nextTokenKind := SkipUntilAnchor(ctx);
        end
        else
            break;
    end;

    RemoveAnchor(rwComma);
    RemoveAnchor(rwCloseParenthesis);
    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);
    RemoveAnchor(pkIdentifier);
    RemoveAnchor(rwOpenParenthesis);

    nextRW := DetermineReservedWord(ctx);
    if nextRW = rwCloseParenthesis then
        TReservedWord.Create(ctx, rwCloseParenthesis, false)
    else
    begin
        state := tsError;
        errorMessage := 'Expected closing parenthesis '')''';
    end;

    if elemType = nil then
        elemType := unknownType;
    typeDef := TArrayTypeDef.Create(ctx, nil, elemType);

    ctx.MarkEndOfToken(Self);
end;

end.
