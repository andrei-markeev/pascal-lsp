unit Designator;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TDesignator = class(TTypedToken)
    public
        designatorToken: TTypedToken;
        constructor Create(ctx: TParserContext; isMaybeLeftHandSide: boolean = false);
    end;

function CreateDesignator(ctx: TParserContext; isMaybeLeftHandSide: boolean = false): TTypedToken;

implementation

uses
    Token, ReservedWord, VarRef, Call, TypeDefs, RoutineTypeDef, TypeDef, Anchors, Symbols, Identifier, Parameters;

function CreateDesignator(ctx: TParserContext; isMaybeLeftHandSide: boolean = false): TTypedToken;
var
    designator: TDesignator;
begin
    designator := TDesignator.Create(ctx, isMaybeLeftHandSide);
    if (designator.designatorToken <> nil) then
    begin
        CreateDesignator := designator.designatorToken;
        designator.state := tsInvisible;
        if designator.endMarker <> nil then
            designator.endMarker.state := tsInvisible;
    end
    else
        CreateDesignator := designator;
end;

function GetUnderlyingSymbol(token: TTypedToken): TSymbol;
begin
    Result := nil;
    if token is TIdentifier then
        Result := TSymbol(TIdentifier(token).symbol)
    else if (token is TVarRef) and (TVarRef(token).firstIdent <> nil) then
        Result := TSymbol(TVarRef(token).firstIdent.symbol);
end;

procedure SetTokenTypeDef(token: TTypedToken; newTypeDef: TTypeDef);
begin
    if token = nil then exit;
    token.typeDef := newTypeDef;
    if token is TIdentifier then
        TIdentifier(token).typeDef := newTypeDef
    else if (token is TVarRef) and (TVarRef(token).firstIdent <> nil) then
        TVarRef(token).firstIdent.typeDef := newTypeDef;
end;

function IsResultVariableRef(ctx: TParserContext; symbol: TSymbol; typeDef: TTypeDef; isMaybeLeftHandSide: boolean): boolean;
var
    routineType: TRoutineTypeDef;
    params: TParameterList;
    nextTokenKind: TTokenKind;
begin
    Result := false;
    if (symbol = nil) or (typeDef = nil) or (symbol.GetCurrentReturnType(ctx) = nil) then
        exit;
    if not (typeDef is TRoutineTypeDef) or (typeDef.kind <> tkFunction) then
        exit;

    if PeekReservedWord(ctx, rwOpenParenthesis) then
        exit;

    // 1. Assignment to function result variable: MyFunc := val
    if PeekReservedWord(ctx, rwAssign) then
        exit(true);

    // 2. Member access on left-hand side: MyFunc.field := val
    nextTokenKind := DetermineNextTokenKind(ctx);
    if isMaybeLeftHandSide and (nextTokenKind.reservedWordKind in [rwDot, rwOpenSquareBracket, rwHat]) then
        exit(true);

    // 3. Routine has required parameters but no '(' was provided: temp := MyFunc
    routineType := TRoutineTypeDef(typeDef);
    params := TParameterList(routineType.parameters);
    if (params <> nil) and (params.GetMinRequiredCount > 0) then
        exit(true);
end;

constructor TDesignator.Create(ctx: TParserContext; isMaybeLeftHandSide: boolean = false);
var
    curToken: TTypedToken;
    oldCursor: PChar;
    symbol: TSymbol;
    returnType: TTypeDef;
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'Designator';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDef := nil;
    designatorToken := nil;

    curToken := CreateVarRef(ctx);
    if curToken = nil then
    begin
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    typeDef := curToken.typeDef;
    symbol := GetUnderlyingSymbol(curToken);

    if IsResultVariableRef(ctx, symbol, typeDef, isMaybeLeftHandSide) then
    begin
        returnType := symbol.GetCurrentReturnType(ctx);
        if returnType = nil then
            returnType := unknownType;
        typeDef := returnType;
        SetTokenTypeDef(curToken, typeDef);
    end;

    while True do
    begin
        if (((typeDef <> nil) and (typeDef.kind = tkFunction) and (typeDef is TRoutineTypeDef)) and not PeekReservedWord(ctx, rwAssign))
           or PeekReservedWord(ctx, rwOpenParenthesis) then
        begin
            curToken := TCall.Create(ctx, curToken);
            if curToken <> nil then
                typeDef := curToken.typeDef;
            continue;
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
        if nextTokenKind.reservedWordKind in [rwDot, rwOpenSquareBracket, rwHat] then
        begin
            oldCursor := ctx.Cursor;
            ctx.SkipTrivia;
            curToken := CreateVarRef(ctx, curToken);
            if curToken <> nil then
                typeDef := curToken.typeDef;

            if ctx.Cursor <= oldCursor then
                break;
        end
        else
            break;
    end;

    designatorToken := curToken;
    ctx.MarkEndOfToken(Self);
end;

end.
