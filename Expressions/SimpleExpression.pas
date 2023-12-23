unit SimpleExpression;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypeDefs, Token, TypedToken, ReservedWord, Term;

type
    TSimpleExpression = class(TTypedToken)
    public
        leftOperand: TTypedToken;
        lastAddOp: TReservedWordKind;
        constructor Create(ctx: TParserContext);
    end;

function CreateSimpleExpression(ctx: TParserContext): TTypedToken;

implementation

function CreateSimpleExpression(ctx: TParserContext): TTypedToken;
var
    simple: TSimpleExpression;
begin
    simple := TSimpleExpression.Create(ctx);
    if simple.lastAddOp = rwUnknown then
    begin
        CreateSimpleExpression := simple.leftOperand;
        simple.state := tsInvisible;
        if simple.endMarker <> nil then
            simple.endMarker.state := tsInvisible;
    end
    else
        CreateSimpleExpression := simple;
end;

constructor TSimpleExpression.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextOperand: TTypedToken;
    str: string;
begin
    ctx.Add(Self);
    tokenName := 'SimpleExpression';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    state := tsCorrect;
    lastAddOp := rwUnknown;
    leftOperand := CreateTerm(ctx);
    typeDef := leftOperand.typeDef;
    nextTokenKind := DetermineNextTokenKind(ctx);

    while nextTokenKind.reservedWordKind in [rwPlus, rwMinus, rwOr, rwXor, rwSymmetricDifference] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastAddOp := nextTokenKind.reservedWordKind;
        nextOperand := CreateTerm(ctx);

        case lastAddOp of
            rwPlus:
                if not (typeDef.kind in [tkString, tkChar, tkCharRange, tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, ctx.Cursor - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected string, char, integer, real or set, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if (typeDef.kind in [tkInteger, tkReal]) and not (nextOperand.typeDef.kind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind in [tkString, tkChar, tkCharRange]) and not (nextOperand.typeDef.kind in [tkString, tkChar, tkCharRange]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected string or char, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkSet) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': ' + str;
                end;
            rwMinus:
                if not (typeDef.kind in [tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, ctx.Cursor - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if (typeDef.kind in [tkInteger, tkReal]) and not (nextOperand.typeDef.kind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkSet) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': ' + str;
                end;
            rwOr, rwXor:
                if not (typeDef.kind in [tkInteger, tkBoolean]) then
                begin
                    state := tsError;
                    SetString(str, start, ctx.Cursor - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or boolean operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if (typeDef.kind = tkInteger) and (nextOperand.typeDef.kind <> tkInteger) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkBoolean) and (nextOperand.typeDef.kind <> tkBoolean) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected boolean, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end;
        end;

        if state = tsError then
            typeDef.kind := tkUnknown
        else if (lastAddOp in [rwPlus, rwMinus]) and (typeDef.kind = tkInteger) and (nextOperand.typeDef.kind = tkReal) then
            typeDef := nextOperand.typeDef // TODO: handle type size expansion
        else if (lastAddOp = rwPlus) and (typeDef.kind in [tkChar, tkCharRange]) and (nextOperand.typeDef.kind in [tkString, tkChar, tkCharRange]) then
            typeDef := shortstringType; // TODO: use a more precise string type

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
