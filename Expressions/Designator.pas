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
    Token, ReservedWord, VarRef, Call, TypeDefs, RoutineTypeDef, TypeDef, Anchors, Symbols, Identifier;

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

constructor TDesignator.Create(ctx: TParserContext; isMaybeLeftHandSide: boolean = false);
var
    curToken: TTypedToken;
    oldCursor: PChar;
    symbol: TSymbol;
    nextTokenKind: TTokenKind;
    returnType: TTypeDef;
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

    if curToken.typeDef <> nil then
        typeDef := curToken.typeDef;

    symbol := nil;
    if curToken is TIdentifier then
        symbol := TSymbol(TIdentifier(curToken).symbol);

    if (symbol <> nil) and isMaybeLeftHandSide then
    begin
        returnType := symbol.GetCurrentReturnType(ctx);
        nextTokenKind := DetermineNextTokenKind(ctx);
        if (returnType <> nil) and (nextTokenKind.reservedWordKind in [rwDot, rwOpenSquareBracket, rwHat]) then
        begin
            typeDef := TRoutineTypeDef(symbol.typeDef).returnType;
            curToken.typeDef := typeDef;
            if curToken is TIdentifier then
                TIdentifier(curToken).typeDef := typeDef;
        end;
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
