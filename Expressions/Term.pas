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
begin
    ctx.Add(Self);
    tokenName := 'Term';

    state := tsCorrect;
    lastMultiplyOp := rwUnknown;

    nextTokenKind := DetermineNextTokenKind(ctx);
    start := ctx.Cursor;
    leftOperand := CreateFactor(ctx, nextTokenKind);
    typeDef := leftOperand.typeDef;

    savedPos := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);
    while nextTokenKind.reservedWordKind in [rwMultiply, rwDivide, rwDiv, rwMod, rwAnd, rwShl, rwShr, rwShl2, rwShr2] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastMultiplyOp := nextTokenKind.reservedWordKind;
        nextTokenKind := DetermineNextTokenKind(ctx);
        nextOperand := CreateFactor(ctx, nextTokenKind);

        if state <> tsError then
        case lastMultiplyOp of
            rwMultiply:
                if not (typeDef.kind in [tkInteger, tkReal, tkSet]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer, real or set operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if (typeDef.kind in [tkInteger, tkReal]) and not (nextOperand.typeDef.kind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkSet) and (nextOperand.typeDef.kind <> tkSet) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected a set, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkSet) and not TypesAreAssignable(typeDef, nextOperand.typeDef, str) then
                begin
                    state := tsError;
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': ' + str;
                end;
            rwDivide:
                if not (typeDef.kind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if not (nextOperand.typeDef.kind in [tkInteger, tkReal]) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or real operands, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end;
            rwDiv, rwMod, rwShl, rwShr, rwShl2, rwShr2:
                if typeDef.kind <> tkInteger then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if nextOperand.typeDef.kind <> tkInteger then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer operands, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end;
            rwAnd:
                if not (typeDef.kind in [tkInteger, tkBoolean]) then
                begin
                    state := tsError;
                    SetString(str, start, savedPos - start);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer or boolean operands, but ' + str + ' is ' + TypeKindStr[ord(typeDef.kind)];
                end
                else if (typeDef.kind = tkInteger) and (nextOperand.typeDef.kind <> tkInteger) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected integer, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end
                else if (typeDef.kind = tkBoolean) and (nextOperand.typeDef.kind <> tkBoolean) then
                begin
                    state := tsError;
                    SetString(str, nextOperand.start, nextOperand.len);
                    errorMessage := 'Cannot apply operator ''' + ReservedWordStr[ord(lastMultiplyOp)] + ''': expected boolean, but ' + str + ' is ' + TypeKindStr[ord(nextOperand.typeDef.kind)];
                end;
        end;

        if state = tsError then
            typeDef.kind := tkUnknown
        else if (lastMultiplyOp in [rwMultiply, rwDivide]) and (typeDef.kind = tkInteger) and (nextOperand.typeDef.kind = tkReal) then
            typeDef := nextOperand.typeDef; // TODO: handle type size expansion

        savedPos := ctx.Cursor;
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
