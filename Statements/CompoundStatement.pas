unit CompoundStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord;

type
    TCompoundStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext; beginRW: TReservedWordKind);
    end;

function CreateCompoundStatement(ctx: TParserContext): TToken;

implementation

uses
    Statement;

function CreateCompoundStatement(ctx: TParserContext): TToken;
begin
    CreateCompoundStatement := TCompoundStatement.Create(ctx, rwBegin);
end;

constructor TCompoundStatement.Create(ctx: TParserContext; beginRW: TReservedWordKind);
var
    nextTokenKind: TTokenKind;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    TReservedWord.Create(ctx, beginRW, false);

    AddAnchor(rwEnd);
    AddAnchor(rwWith);
    AddAnchor(rwFor);
    AddAnchor(rwCase);
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwTry);
    AddAnchor(rwGoto);
    AddAnchor(rwBegin);
    AddAnchor(rwInherited);
    AddAnchor(rwInitialization);
    AddAnchor(rwFinalization);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwTry, rwGoto, rwBegin, rwInherited])
          or (nextTokenKind.primitiveKind = pkIdentifier)
    do
    begin
        CreateStatement(ctx);
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        if PeekReservedWord(ctx, rwSemiColon) then
            TReservedWord.Create(ctx, rwSemiColon, false)
        else if not (nextTokenKind.reservedWordKind in [rwEnd, rwUntil, rwElse, rwOtherwise, rwExcept, rwFinally, rwInitialization, rwFinalization]) then
            TReservedWord.Create(ctx, rwSemiColon, false);

        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwWith);
    RemoveAnchor(rwFor);
    RemoveAnchor(rwCase);
    RemoveAnchor(rwIf);
    RemoveAnchor(rwWhile);
    RemoveAnchor(rwRepeat);
    RemoveAnchor(rwTry);
    RemoveAnchor(rwGoto);
    RemoveAnchor(rwBegin);
    RemoveAnchor(rwInherited);
    RemoveAnchor(rwInitialization);
    RemoveAnchor(rwFinalization);
    RemoveAnchor(pkIdentifier);

    if (beginRW = rwBegin) or (nextTokenKind.reservedWordKind <> rwFinalization) then
        TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
