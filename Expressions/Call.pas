unit Call;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, TypedToken;

type
    TCall = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; ref: TTypedToken);
    end;

implementation

uses
    sysutils, TypeDefs, ReservedWord, Expression, VarRef;

constructor TCall.Create(ctx: TParserContext; ref: TTypedToken);
var
    expr: TTypedToken;
    params: TTypedTokenArray;
    i: integer;
begin    
    ctx.InsertBefore(ref, Self);
    tokenName := 'Call';
    start := ref.start;
    state := tsCorrect;

    typeDef := ref.typeDef.returnType^;
    params := TTypedTokenArray(ref.typeDef.parameters);

    if PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, false);

        for i := 0 to params.count - 1 do
        begin

            if PeekReservedWord(ctx, rwCloseParenthesis) then
            begin
                state := tsError;
                errorMessage := 'Expected ' + IntToStr(params.count) + ' parameters, but got ' + IntToStr(i);
                break;
            end;

            expr := CreateExpression(ctx);

            if not TypesAreAssignable(params.items[i].typeDef, expr.typeDef, expr.errorMessage) then
            begin
                expr.state := tsError;
                expr.errorMessage := 'Invalid parameter: ' + expr.errorMessage;
            end;

            if i <> params.count - 1 then
                TReservedWord.Create(ctx, rwComma, false);
        end;

        TReservedWord.Create(ctx, rwCloseParenthesis, false);
    end
    // TODO: count required parameters only, some might be optional
    else if params.count > 0 then
    begin
        state := tsError;
        errorMessage := 'Procedure call requires ' + IntToStr(params.count) + ' parameters!';
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
