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

uses TypeDef;

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
    savedPos: PChar;
    myKind, nextKind: TTypeKind;
begin
    ctx.Add(Self);
    tokenName := 'SimpleExpression';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    state := tsCorrect;
    lastAddOp := rwUnknown;
    leftOperand := CreateTerm(ctx);
    if (leftOperand <> nil) and (leftOperand.typeDef <> nil) then
        typeDef := leftOperand.typeDef
    else
        typeDef := unknownType;

    savedPos := ctx.Cursor;
    nextTokenKind := DetermineNextTokenKind(ctx);

    while nextTokenKind.reservedWordKind in [rwPlus, rwMinus, rwOr, rwXor, rwSymmetricDifference] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastAddOp := nextTokenKind.reservedWordKind;
        nextOperand := CreateTerm(ctx);

        if (typeDef <> nil) then
            myKind := typeDef.kind
        else
            myKind := tkUnknown;

        if (nextOperand <> nil) and (nextOperand.typeDef <> nil) then
            nextKind := nextOperand.typeDef.kind
        else
            nextKind := tkUnknown;

        if (state <> tsError) and (nextOperand <> nil) then
        case lastAddOp of
            rwPlus:
                if not (myKind in [tkString, tkChar, tkCharRange, tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected string, char, integer, real or set, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if (myKind in [tkInteger, tkReal]) and not (nextKind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind in [tkString, tkChar, tkCharRange]) and not (nextKind in [tkString, tkChar, tkCharRange]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected string or char, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkSet) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': ' + str;
                end;
            rwMinus:
                if not (myKind in [tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if (myKind in [tkInteger, tkReal]) and not (nextKind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkSet) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': ' + str;
                end;
            rwOr, rwXor:
                if not (myKind in [tkInteger, tkBoolean]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer or boolean operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if (myKind = tkInteger) and (nextKind <> tkInteger) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected integer, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkBoolean) and (nextKind <> tkBoolean) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected boolean, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end;
            rwSymmetricDifference:
                if (myKind <> tkSet) or (nextKind <> tkSet) then
                begin
                    state := tsError;
                    if myKind <> tkSet then
                        SetString(str, start, savedPos - start)
                    else
                        SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': expected set operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastAddOp)] + ''': ' + str;
                end;
        end;

        if state = tsError then
            typeDef := unknownType
        else if (nextOperand <> nil) and (nextOperand.typeDef <> nil) and (lastAddOp in [rwPlus, rwMinus]) and (myKind = tkInteger) and (nextKind = tkReal) then
            typeDef := nextOperand.typeDef // TODO: handle type size expansion
        else if (nextOperand <> nil) and (nextOperand.typeDef <> nil) and (lastAddOp = rwPlus) and (myKind in [tkChar, tkCharRange]) and (nextKind in [tkString, tkChar, tkCharRange]) then
            typeDef := shortstringType; // TODO: use a more precise string type

        savedPos := ctx.Cursor;
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
