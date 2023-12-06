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
        constructor Create(ctx: TParserContext; createExpression: TCreateTokenFunc);
    end;

function CreateSimpleExpression(ctx: TParserContext; createExpression: TCreateTokenFunc): TTypedToken;

implementation

function CreateSimpleExpression(ctx: TParserContext; createExpression: TCreateTokenFunc): TTypedToken;
var
    simple: TSimpleExpression;
begin
    simple := TSimpleExpression.Create(ctx, createExpression);
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

constructor TSimpleExpression.Create(ctx: TParserContext; createExpression: TCreateTokenFunc);
var
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'SimpleExpression';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    lastAddOp := rwUnknown;
    leftOperand := CreateTerm(ctx, createExpression);
    typeDef := leftOperand.typeDef;
    nextTokenKind := DetermineNextTokenKind(ctx);

    while nextTokenKind.reservedWordKind in [rwPlus, rwMinus, rwOr, rwXor] do
    begin
        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
        lastAddOp := nextTokenKind.reservedWordKind;
        CreateTerm(ctx, createExpression);
        // TODO: determine type
        // TODO: type compatibility checks
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
