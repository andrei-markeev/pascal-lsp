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
        constructor Create(ctx: TParserContext);
    end;

function CreateDesignator(ctx: TParserContext): TTypedToken;

implementation

uses
    Token, ReservedWord, VarRef, Call, TypeDefs, RoutineTypeDef, TypeDef, Anchors;

function CreateDesignator(ctx: TParserContext): TTypedToken;
var
    designator: TDesignator;
begin
    designator := TDesignator.Create(ctx);
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

constructor TDesignator.Create(ctx: TParserContext);
var
    currToken: TTypedToken;
    oldCursor: PChar;
begin
    ctx.Add(Self);
    tokenName := 'Designator';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDef := nil;
    designatorToken := nil;

    currToken := CreateVarRef(ctx);
    if currToken = nil then
    begin
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    if currToken.typeDef <> nil then
        typeDef := currToken.typeDef;

    while True do
    begin
        if (((typeDef <> nil) and (typeDef.kind = tkFunction) and (typeDef is TRoutineTypeDef)) and not PeekReservedWord(ctx, rwAssign))
           or PeekReservedWord(ctx, rwOpenParenthesis) then
        begin
            currToken := TCall.Create(ctx, currToken);
            if currToken <> nil then
                typeDef := currToken.typeDef;
            continue;
        end;

        if DetermineNextTokenKind(ctx).reservedWordKind in [rwDot, rwOpenSquareBracket, rwHat] then
        begin
            oldCursor := ctx.Cursor;
            ctx.SkipTrivia;
            currToken := CreateVarRef(ctx, currToken);
            if currToken <> nil then
                typeDef := currToken.typeDef;

            if ctx.Cursor <= oldCursor then
                break;
        end
        else
            break;
    end;

    designatorToken := currToken;
    ctx.MarkEndOfToken(Self);
end;

end.
