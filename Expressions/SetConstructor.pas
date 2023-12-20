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
    TypeDefs, Token, ReservedWord, EnumSpec, Expression;

constructor TSetConstructor.Create(ctx: TParserContext);
var
    expr: TTypedToken;
    exprStr: string;
    nextReservedWord: TReservedWordKind;
    baseType: PTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'SetConstructor';
    start := ctx.Cursor;
    state := tsCorrect;

    TReservedWord.Create(ctx, rwOpenSquareBracket, true);

    expr := CreateExpression(ctx);

    if expr.typeDef.kind = tkEnumMember then
        baseType := @TEnumSpec(expr.typeDef.enumSpec).typeDef
    else if expr.typeDef.kind = tkCharRange then
        baseType := @charType
    else
        baseType := @expr.typeDef;

    typeDef.size := 1;
    typeDef.kind := tkSet;
    typeDef.typeOfSet := baseType;

    if not (baseType^.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        errorMessage := 'Expected a set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(baseType^.kind)];
    end
    else if baseType^.size > 1 then
    begin
        state := tsError;
        errorMessage := 'The base type of the set must not have more than 256 possible values!';
    end;

    nextReservedWord := DetermineReservedWord(ctx);
    while nextReservedWord in [rwComma, rwRange] do
    begin
        TReservedWord.Create(ctx, nextReservedWord, true);
        expr := CreateExpression(ctx);
        if not TypesAreAssignable(baseType^, expr.typeDef, errorMessage) then
        begin
            state := tsError;
            SetString(exprStr, expr.start, expr.len);
            errorMessage := exprStr + ' is not assignable to the type of the set (' + TypeKindStr[ord(baseType^.kind)] + '): ' + errorMessage;
        end;
        nextReservedWord := DetermineReservedWord(ctx);
    end;

    TReservedWord.Create(ctx, rwCloseSquareBracket, false);

    ctx.MarkEndOfToken(Self);
end;

end.
