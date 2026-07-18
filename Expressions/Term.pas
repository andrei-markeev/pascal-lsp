unit Term;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypeDefs, Token, TypedToken, ReservedWord, Factor;

type
    TTerm = class(TTypedToken)
    public
        leftOperand: TTypedToken;
        lastMultiplyOp: TReservedWordKind;
        constructor Create(ctx: TParserContext);
    end;

function CreateTerm(ctx: TParserContext): TTypedToken;

implementation

uses TypeDef;

function CreateTerm(ctx: TParserContext): TTypedToken;
var
    newTerm: TTerm;
begin
    newTerm := TTerm.Create(ctx);
    if newTerm.lastMultiplyOp = rwUnknown then
    begin
        CreateTerm := newTerm.leftOperand;
        newTerm.state := tsInvisible;
        if newTerm.endMarker <> nil then
            newTerm.endMarker.state := tsInvisible;
    end
    else
        CreateTerm := newTerm;
end;

constructor TTerm.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextOperand: TTypedToken;
    str: string;
    savedPos: PChar;
    myKind, nextKind: TTypeKind;
begin
    ctx.Add(Self);
    tokenName := 'Term';

    state := tsCorrect;
    lastMultiplyOp := rwUnknown;

    nextTokenKind := DetermineNextTokenKind(ctx);
    start := ctx.Cursor;
    leftOperand := CreateFactor(ctx, nextTokenKind);
    if (leftOperand <> nil) and (leftOperand.typeDef <> nil) then
        typeDef := leftOperand.typeDef
    else
        typeDef := unknownType;

    savedPos := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);
    while nextTokenKind.reservedWordKind in [rwMultiply, rwDivide, rwDiv, rwMod, rwAnd, rwShl, rwShr, rwShl2, rwShr2] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastMultiplyOp := nextTokenKind.reservedWordKind;
        nextTokenKind := DetermineNextTokenKind(ctx);
        nextOperand := CreateFactor(ctx, nextTokenKind);

        if typeDef <> nil then
            myKind := typeDef.kind
        else
            myKind := tkUnknown;

        if (nextOperand <> nil) and (nextOperand.typeDef <> nil) then
            nextKind := nextOperand.typeDef.kind
        else
            nextKind := tkUnknown;

        if (state <> tsError) and (nextOperand <> nil) then
        case lastMultiplyOp of
            rwMultiply:
                if not (myKind in [tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer, real or set operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if (myKind in [tkInteger, tkReal]) and not (nextKind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkSet) and (nextKind <> tkSet) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected a set, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkSet) and (nextOperand.typeDef <> nil) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': ' + str;
                end;
            rwDivide:
                if not (myKind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if not (nextKind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end;
            rwDiv, rwMod, rwShl, rwShr, rwShl2, rwShr2:
                if myKind <> tkInteger then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if nextKind <> tkInteger then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer operands, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end;
            rwAnd:
                if not (myKind in [tkInteger, tkBoolean]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or boolean operands, but ' + str + ' is ' + TypeKindStr[ord(myKind)];
                end
                else if (myKind = tkInteger) and (nextKind <> tkInteger) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end
                else if (myKind = tkBoolean) and (nextKind <> tkBoolean) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected boolean, but ' + str + ' is ' + TypeKindStr[ord(nextKind)];
                end;
        end;

        if state = tsError then
            typeDef := unknownType
        else if (nextOperand <> nil) and (nextOperand.typeDef <> nil) and (lastMultiplyOp in [rwMultiply, rwDivide]) and (myKind = tkInteger) and (nextKind = tkReal) then
            typeDef := nextOperand.typeDef; // TODO: handle type size expansion

        savedPos := ctx.Cursor;
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
