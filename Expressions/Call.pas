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
    sysutils, classes, TypeDefs, Parameters, ReservedWord, Expression, RoutineTypeDef,
    Identifier, VarRef, CompilationMode, Symbols;

function IsExitRef(refToken: TTypedToken): boolean;
begin
    if refToken is TIdentifier then
        exit(SameText(TIdentifier(refToken).name, 'Exit'))
    else
        exit(false);
end;

function IsProcedureRef(exprToken: TTypedToken; ctx: TParserContext): boolean;
var
    sym: TSymbol;
begin
    if exprToken = nil then exit(false);

    if exprToken is TIdentifier then
    begin
        sym := FindSymbol(TIdentifier(exprToken).name, ctx.Cursor);
        exit((sym <> nil) and (sym.kind = skProcedure));
    end
    else if exprToken is TVarRef then
    begin
        sym := TVarRef(exprToken).symbol;
        exit((sym <> nil) and (sym.kind = skProcedure));
    end;

    exit(false);
end;

constructor TCall.Create(ctx: TParserContext; ref: TTypedToken);
var
    expr: TTypedToken;
    params: TParameterList;
    overloads: TFPList;
    n, match: integer;
    hasMoreParams, hasMatch: boolean;
    paramError: string;
    isExitCall: boolean;
begin    
    ctx.InsertBefore(ref, Self);
    tokenName := 'Call';
    start := ref.start;
    state := tsCorrect;
    isExitCall := (mfExitProcName in Features[ctx.mode]) and IsExitRef(ref);

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

            if isExitCall and (n = 0) and IsProcedureRef(expr, ctx) then
            begin
                expr.state := tsCorrect;
                expr.errorMessage := '';
            end;

            if params <> nil then
            begin
                hasMatch := false;
                while not hasMatch do
                begin
                    if params.count <= n then
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
                    end
                    else if (expr <> nil) and (params.items[n].typeDef <> nil) and not TypesAreAssignable(ctx, params.items[n].typeDef, expr.typeDef, paramError) then
                    begin
                        inc(match);
                        if (overloads = nil) or (match >= overloads.Count) then
                        begin
                            expr.state := tsError;
                            if expr.errorMessage <> '' then
                                expr.errorMessage := 'Invalid parameter: ' + expr.errorMessage
                            else if paramError <> '' then
                                expr.errorMessage := 'Invalid parameter: ' + paramError
                            else
                                expr.errorMessage := 'Invalid parameter.';
                            break;
                        end;
                        params := TParameterList(TRoutineTypeDef(overloads.Items[match]).parameters);
                    end
                    else
                        hasMatch := true;
                end;
            end;

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
    while (n < params.GetMinRequiredCount) or (n > params.count) do
    begin
        inc(match);
        if (overloads = nil) or (match >= overloads.Count) then
        begin
            state := tsError;
            if params.GetMinRequiredCount = params.count then
                errorMessage := 'Expected ' + IntToStr(params.count) + ' parameters, but got ' + IntToStr(n)
            else
                errorMessage := 'Expected at least ' + IntToStr(params.GetMinRequiredCount) + ' parameters, but got ' + IntToStr(n);
            break;
        end;
        params := TParameterList(TRoutineTypeDef(overloads.Items[match]).parameters);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
