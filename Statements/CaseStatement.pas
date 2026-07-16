unit CaseStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord;

type
    TCaseStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Expression, CaseBranch, Statement;

constructor TCaseStatement.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'Case';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwCase) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwCase, true);
    CreateExpression(ctx);
    TReservedWord.Create(ctx, rwOf, false);

    AddAnchor(rwEnd);
    AddAnchor(rwElse);
    AddAnchor(pkNumber);
    AddAnchor(pkString);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.primitiveKind in [pkNumber, pkString, pkIdentifier]) do
    begin
        TCaseBranch.Create(ctx);
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        TReservedWord.Create(ctx, rwSemiColon, false);
        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);

    if nextTokenKind.reservedWordKind = rwElse then
    begin
        TReservedWord.Create(ctx, rwElse, true);

        AddAnchor(rwWith);
        AddAnchor(rwFor);
        AddAnchor(rwCase);
        AddAnchor(rwIf);
        AddAnchor(rwWhile);
        AddAnchor(rwRepeat);
        AddAnchor(rwGoto);
        AddAnchor(rwBegin);

        nextTokenKind := SkipUntilAnchor(ctx);
        while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwGoto, rwBegin])
              or (nextTokenKind.primitiveKind = pkIdentifier)
        do
        begin
            CreateStatement(ctx);
            AddAnchor(rwSemiColon);
            nextTokenKind := SkipUntilAnchor(ctx);
            RemoveAnchor(rwSemiColon);
            TReservedWord.Create(ctx, rwSemiColon, false);
            nextTokenKind := SkipUntilAnchor(ctx);
        end;

        RemoveAnchor(rwWith);
        RemoveAnchor(rwFor);
        RemoveAnchor(rwCase);
        RemoveAnchor(rwIf);
        RemoveAnchor(rwWhile);
        RemoveAnchor(rwRepeat);
        RemoveAnchor(rwGoto);
        RemoveAnchor(rwBegin);
    end;

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwElse);
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwEnd, false);
    ctx.MarkEndOfToken(Self);
end;

end.
