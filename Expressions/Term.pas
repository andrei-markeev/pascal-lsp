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
begin
    ctx.Add(Self);
    tokenName := 'Term';

    lastMultiplyOp := rwUnknown;

    nextTokenKind := DetermineNextTokenKind(ctx);
    start := ctx.Cursor;
    leftOperand := CreateFactor(ctx, nextTokenKind);
    typeDef := leftOperand.typeDef;

    nextTokenKind := DetermineNextTokenKind(ctx);
    while nextTokenKind.reservedWordKind in [rwMultiply, rwDivide, rwDiv, rwMod, rwAnd, rwShl, rwShr, rwShl2, rwShr2] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastMultiplyOp := nextTokenKind.reservedWordKind;
        nextTokenKind := DetermineNextTokenKind(ctx);
        CreateFactor(ctx, nextTokenKind);
        // TODO: determine type

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    state := tsCorrect;

    // TODO: type compatibility checks

    ctx.MarkEndOfToken(Self);
end;

end.
