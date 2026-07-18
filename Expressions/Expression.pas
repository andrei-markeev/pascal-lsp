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

uses TypeDef, EnumMemberTypeDef;

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
    leftKind, rightKind: TTypeKind;
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
        if (leftOperand <> nil) and (leftOperand.typeDef <> nil) then
            typeDef := leftOperand.typeDef
        else
            typeDef := unknownType;
    end;

    state := tsCorrect;

    if (leftOperand <> nil) and (leftOperand.typeDef <> nil) and (rightOperand <> nil) and (rightOperand.typeDef <> nil) then
    begin
        leftKind := leftOperand.typeDef.kind;
        rightKind := rightOperand.typeDef.kind;

        if relationalOp in [rwLess, rwLessOrEqual, rwMore, rwMoreOrEqual] then
        begin
            typesAreCompatible := false;
            if (leftKind in [tkReal, tkInteger]) and (rightKind in [tkReal, tkInteger]) then
                typesAreCompatible := true;
            if (leftKind in [tkString, tkChar, tkCharRange]) and (rightKind in [tkString, tkChar, tkCharRange]) then
                typesAreCompatible := true;
            if (leftKind = tkEnumMember) and (rightKind = tkEnumMember) then
                typesAreCompatible := (leftOperand.typeDef is TEnumMemberTypeDef) and (rightOperand.typeDef is TEnumMemberTypeDef) and (TEnumMemberTypeDef(leftOperand.typeDef).enumSpec = TEnumMemberTypeDef(rightOperand.typeDef).enumSpec);

            if not typesAreCompatible then
            begin
                state := tsError;
                if leftKind <> rightKind then
                    errorMessage := 'Comparing ' + TypeKindStr[ord(leftKind)] + ' with ' + TypeKindStr[ord(rightKind)] + ' is not supported!'
                else
                    errorMessage := 'Comparing enum values from different enums is not supported! You may use Ord function to compare their numeric representations, if this was the intention.';
            end;
        end
        else if relationalOp = rwIn then
        begin
            if not (leftKind in [tkInteger, tkEnum, tkEnumMember, tkChar, tkCharRange]) then
            begin
                state := tsError;
                errorMessage := 'Value of type ' + TypeKindStr[ord(leftKind)] + ' cannot belong to a set! Only ordinal types are supported, such as integer, enumeration and char.';
            end;

            if rightKind <> tkSet then
            begin
                state := tsError;
                errorMessage := 'Right operand of type set expected, found ' + TypeKindStr[ord(rightKind)] + '!';
            end;
        end;
    end;
    // TODO: more type compatibility checks

    ctx.MarkEndOfToken(Self);
end;

end.
