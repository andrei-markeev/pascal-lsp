unit Call;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypedToken;

type
    TCall = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; ref: TTypedToken);
    end;

implementation

uses
    sysutils, classes, TypeDefs, Parameters, ReservedWord, Expression;

constructor TCall.Create(ctx: TParserContext; ref: TTypedToken);
var
    expr: TTypedToken;
    params: TParameterList;
    overloads: TFPList;
    n, match: integer;
    hasMoreParams: boolean;
begin    
    ctx.InsertBefore(ref, Self);
    tokenName := 'Call';
    start := ref.start;
    state := tsCorrect;

    match := -1;
    overloads := TFPList(ref.typeDef.overloads);

    params := TParameterList(ref.typeDef.parameters);
    if ref.typeDef.returnType <> nil then
        typeDef := ref.typeDef.returnType^
    else
        typeDef := unknownType;

    n := 0;

    if PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, false);

        if not PeekReservedWord(ctx, rwCloseParenthesis) then
        repeat

            expr := CreateExpression(ctx);

            repeat

                while params.count <= n do
                begin
                    inc(match);
                    if (overloads = nil) or (match >= overloads.Count) then
                    begin
                        expr.state := tsError;
                        expr.errorMessage := 'Too many parameters.';
                        TReservedWord.Create(ctx, rwCloseParenthesis, false);
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                    params := TParameterList(PTypeDef(overloads.Items[match])^.parameters);
                end;

                if not TypesAreAssignable(params.items[n].typeDef^, expr.typeDef, expr.errorMessage) then
                begin
                    inc(match);
                    if (overloads = nil) or (match >= overloads.Count) then
                    begin
                        expr.state := tsError;
                        expr.errorMessage := 'Invalid parameter: ' + expr.errorMessage;
                        break;
                    end;
                    params := TParameterList(PTypeDef(overloads.Items[match])^.parameters);
                end;

            until params.count > n;

            inc(n);

            if PeekReservedWord(ctx, rwCloseParenthesis) then
                break;

            hasMoreParams := PeekReservedWord(ctx, rwComma);
            if hasMoreParams then
                TReservedWord.Create(ctx, rwComma, true);

        until not hasMoreParams;

        TReservedWord.Create(ctx, rwCloseParenthesis, false);
    end;

    while params.count <> n do
    begin
        inc(match);
        if (overloads = nil) or (match >= overloads.Count) then
        begin
            state := tsError;
            errorMessage := 'Expected ' + IntToStr(params.count) + ' parameters, but got ' + IntToStr(n);
            break;
        end;
        params := TParameterList(PTypeDef(overloads.Items[match])^.parameters);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
