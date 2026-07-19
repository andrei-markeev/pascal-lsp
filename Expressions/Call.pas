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
    sysutils, classes, TypeDefs, Parameters, ReservedWord, Expression, RoutineTypeDef;

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
    if (ref <> nil) and (ref.typeDef is TRoutineTypeDef) then
    begin
        overloads := TRoutineTypeDef(ref.typeDef).overloads;
        params := TParameterList(TRoutineTypeDef(ref.typeDef).parameters);
        if TRoutineTypeDef(ref.typeDef).returnType <> nil then
            typeDef := TRoutineTypeDef(ref.typeDef).returnType
        else
            typeDef := unknownType;
    end
    else
    begin
        overloads := nil;
        params := nil;
        typeDef := unknownType;
        if (ref <> nil) and (ref.state <> tsError) and (ref.typeDef <> nil) and (ref.typeDef <> unknownType) then
        begin
            state := tsError;
            if PeekReservedWord(ctx, rwOpenParenthesis) then
                errorMessage := 'Cannot call expression because it is not a procedure or function!'
            else
                errorMessage := 'Only procedure calls and assignments can be used as statements!';
        end;
    end;

    n := 0;

    if PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, false);

        if not PeekReservedWord(ctx, rwCloseParenthesis) then
        repeat

            expr := CreateExpression(ctx);

            if params <> nil then
            repeat

                while params.count <= n do
                begin
                    inc(match);
                    if (overloads = nil) or (match >= overloads.Count) then
                    begin
                        if expr <> nil then
                        begin
                            expr.state := tsError;
                            expr.errorMessage := 'Too many parameters.';
                        end;
                        TReservedWord.Create(ctx, rwCloseParenthesis, false);
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                    params := TParameterList(TRoutineTypeDef(overloads.Items[match]).parameters);
                end;

                if (expr <> nil) and (params.items[n].typeDef <> nil) and not TypesAreAssignable(params.items[n].typeDef, expr.typeDef, expr.errorMessage) then
                begin
                    inc(match);
                    if (overloads = nil) or (match >= overloads.Count) then
                    begin
                        expr.state := tsError;
                        expr.errorMessage := 'Invalid parameter: ' + expr.errorMessage;
                        break;
                    end;
                    params := TParameterList(TRoutineTypeDef(overloads.Items[match]).parameters);
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

    if params <> nil then
    while params.count <> n do
    begin
        inc(match);
        if (overloads = nil) or (match >= overloads.Count) then
        begin
            state := tsError;
            errorMessage := 'Expected ' + IntToStr(params.count) + ' parameters, but got ' + IntToStr(n);
            break;
        end;
        params := TParameterList(TRoutineTypeDef(overloads.Items[match]).parameters);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
