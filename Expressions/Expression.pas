unit Expression;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypeDefs, Token, TypedToken, ReservedWord, SimpleExpression;

type
    TExpression = class(TTypedToken)
    public
        leftOperand: TTypedToken;
        relationalOp: TReservedWordKind;
        rightOperand: TTypedToken;
        constructor Create(ctx: TParserContext);
    end;

function CreateExpression(ctx: TParserContext): TTypedToken;

implementation

function CreateExpression(ctx: TParserContext): TTypedToken;
var
    expr: TExpression;
begin
    expr := TExpression.Create(ctx);
    if expr.relationalOp = rwUnknown then
    begin
        CreateExpression := expr.leftOperand;
        expr.state := tsInvisible;
        if expr.endMarker <> nil then
            expr.endMarker.state := tsInvisible;
    end
    else
        CreateExpression := expr;
end;

constructor TExpression.Create(ctx: TParserContext);
var
    typesAreCompatible: boolean;
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'Expression';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    leftOperand := CreateSimpleExpression(ctx);
    nextTokenKind := DetermineNextTokenKind(ctx);
    if nextTokenKind.reservedWordKind in [rwEquals, rwNotEqual, rwMore, rwMoreOrEqual, rwLess, rwLessOrEqual, rwIn, rwIs] then
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        relationalOp := nextTokenKind.reservedWordKind;
        rightOperand := CreateSimpleExpression(ctx);
        typeDef := booleanType;
    end
    else
    begin
        relationalOp := rwUnknown;
        rightOperand := nil;
        typeDef := leftOperand.typeDef;
    end;

    state := tsCorrect;

    if relationalOp in [rwLess, rwLessOrEqual, rwMore, rwMoreOrEqual] then
    begin
        typesAreCompatible := false;
        if (leftOperand.typeDef.kind in [tkReal, tkInteger]) and (rightOperand.typeDef.kind in [tkReal, tkInteger]) then
            typesAreCompatible := true;
        if (leftOperand.typeDef.kind in [tkString, tkChar, tkCharRange]) and (rightOperand.typeDef.kind in [tkString, tkChar, tkCharRange]) then
            typesAreCompatible := true;
        if (leftOperand.typeDef.kind = tkEnumMember) and (rightOperand.typeDef.kind = tkEnumMember) then
            typesAreCompatible := leftOperand.typeDef.enumSpec = rightOperand.typeDef.enumSpec;

        if not typesAreCompatible then
        begin
            state := tsError;
            if leftOperand.typeDef.kind <> rightOperand.typeDef.kind then
                errorMessage := 'Comparing ' + TypeKindStr[ord(leftOperand.typeDef.kind)] + ' with ' + TypeKindStr[ord(rightOperand.typeDef.kind)] + ' is not supported!'
            else
                errorMessage := 'Comparing enum values from different enums is not supported! You may use Ord function to compare their numeric representations, if this was the intention.';
        end;
    end
    else if relationalOp = rwIn then
    begin

        if not (leftOperand.typeDef.kind in [tkInteger, tkEnumMember, tkChar, tkCharRange]) then
        begin
            state := tsError;
            errorMessage := 'Value of type ' + TypeKindStr[ord(leftOperand.typeDef.kind)] + ' cannot belong to a set! Only ordinal types are supported, such as integer, enumeration and char.';
        end;

        if rightOperand.typeDef.kind <> tkSet then
        begin
            state := tsError;
            errorMessage := 'Right operand of type set expected, found ' + TypeKindStr[ord(rightOperand.typeDef.kind)] + '!';
        end;
    end;
    // TODO: more type compatibility checks

    ctx.MarkEndOfToken(Self);
end;

end.
