unit SetConstructor;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TSetConstructor = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDef, TypeDefs, Token, ReservedWord, Expression, SetTypeDef, EnumMemberTypeDef;

constructor TSetConstructor.Create(ctx: TParserContext);
var
    expr: TTypedToken;
    exprStr: string;
    nextReservedWord: TReservedWordKind;
    baseType: TTypeDef;
    setTypeDef: TSetTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'SetConstructor';
    start := ctx.Cursor;
    state := tsCorrect;

    TReservedWord.Create(ctx, rwOpenSquareBracket, true);

    nextReservedWord := DetermineReservedWord(ctx);

    if nextReservedWord = rwCloseSquareBracket then
    begin
        typeDef := TSetTypeDef.Create(ctx, unknownType, 1);
        TReservedWord.Create(ctx, rwCloseSquareBracket, false);
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    expr := CreateExpression(ctx);

    if (expr <> nil) and (expr.typeDef <> nil) and (expr.typeDef.kind = tkEnumMember) and (expr.typeDef is TEnumMemberTypeDef) then
        baseType := TEnumMemberTypeDef(expr.typeDef).enumType
    else if (expr <> nil) and (expr.typeDef <> nil) and (expr.typeDef.kind = tkCharRange) then
        baseType := charType
    else if (expr <> nil) and (expr.typeDef <> nil) then
        baseType := expr.typeDef
    else
        baseType := unknownType;

    setTypeDef := TSetTypeDef.Create(ctx, baseType, 1);
    typeDef := setTypeDef;

    if (baseType = nil) or not (baseType.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        setTypeDef.typeOfSet := unknownType;
        if baseType <> nil then
            errorMessage := 'Expected a set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(baseType.kind)]
        else
            errorMessage := 'Expected a set of ordinal type.';
    end;

    nextReservedWord := DetermineReservedWord(ctx);
    while nextReservedWord in [rwComma, rwRange] do
    begin
        TReservedWord.Create(ctx, nextReservedWord, true);
        expr := CreateExpression(ctx);
        if (expr <> nil) and (expr.typeDef <> nil) and (state = tsCorrect) and not TypesAreAssignable(baseType, expr.typeDef, errorMessage) then
        begin
            setTypeDef.typeOfSet := unknownType;
            state := tsError;
            SetString(exprStr, expr.start, expr.len);
            if baseType <> nil then
                errorMessage := exprStr + ' is not assignable to the type of the set (' + TypeKindStr[ord(baseType.kind)] + '): ' + errorMessage
            else
                errorMessage := exprStr + ' is not assignable to the type of the set: ' + errorMessage;
        end;
        // TODO: check that range has lower element first (if expressions are constants)
        nextReservedWord := DetermineReservedWord(ctx);
    end;

    TReservedWord.Create(ctx, rwCloseSquareBracket, false);

    ctx.MarkEndOfToken(Self);
end;

end.
