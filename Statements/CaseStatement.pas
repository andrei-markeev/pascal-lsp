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
    CompilationMode, Expression, CaseBranch, Statement, BranchTracker, Symbols, VarRef, TypedToken, Identifier;

procedure ExtractCaseSelector(exprToken: TTypedToken; out baseSym: TSymbol; out tagSym: TSymbol);
var
    ref: TVarRef;
begin
    baseSym := nil;
    tagSym := nil;

    if exprToken is TVarRef then
    begin
        ref := TVarRef(exprToken);
        tagSym := ref.symbol;
        if (ref.firstIdent <> nil) and (ref.firstIdent.symbol <> nil) then
            baseSym := TSymbol(ref.firstIdent.symbol)
        else
            baseSym := ref.symbol;
    end;
end;

constructor TCaseStatement.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    fallbackRW: TReservedWord;
    prevCursor: PChar;
    caseExpr: TTypedToken;
    baseSym: TSymbol;
    tagSym: TSymbol;
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
    caseExpr := CreateExpression(ctx);
    ExtractCaseSelector(caseExpr, baseSym, tagSym);
    PushCaseStatement(baseSym, tagSym);

    TReservedWord.Create(ctx, rwOf, false);

    AddAnchor(rwEnd);
    AddAnchor(rwElse);
    AddAnchor(rwOtherwise);
    AddAnchor(pkNumber);
    AddAnchor(pkString);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.primitiveKind in [pkNumber, pkString, pkIdentifier]) do
    begin
        prevCursor := ctx.Cursor;
        TCaseBranch.Create(ctx);
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        if PeekReservedWord(ctx, rwSemiColon) then
            TReservedWord.Create(ctx, rwSemiColon, false)
        else if not (nextTokenKind.reservedWordKind in [rwEnd, rwUntil, rwElse, rwOtherwise, rwExcept, rwFinally, rwInitialization, rwFinalization]) then
            TReservedWord.Create(ctx, rwSemiColon, false);
        nextTokenKind := SkipUntilAnchor(ctx);
        EnsureCursorAdvanced(ctx, prevCursor, nextTokenKind);
    end;

    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);

    if (nextTokenKind.reservedWordKind = rwElse) or (nextTokenKind.reservedWordKind = rwOtherwise) then
    begin
        if nextTokenKind.reservedWordKind = rwElse then
        begin
            fallbackRW := TReservedWord.Create(ctx, rwElse, true);
            if not (mfCaseElseClause in Features[ctx.mode]) and not (mfCaseOtherwiseClause in Features[ctx.mode]) then
            begin
                fallbackRW.state := tsError;
                fallbackRW.errorMessage := 'Fallback block not supported in Standard Pascal (ISO 7185)';
            end
            else if not (mfCaseElseClause in Features[ctx.mode]) and (mfCaseOtherwiseClause in Features[ctx.mode]) then
            begin
                fallbackRW.state := tsError;
                fallbackRW.errorMessage := 'Use ''otherwise'' instead of ''else'' in Extended Pascal (ISO 10206)';
            end;
        end
        else
            TReservedWord.Create(ctx, rwOtherwise, true);

        AddAnchor(rwWith);
        AddAnchor(rwFor);
        AddAnchor(rwCase);
        AddAnchor(rwIf);
        AddAnchor(rwWhile);
        AddAnchor(rwRepeat);
        AddAnchor(rwGoto);
        AddAnchor(rwBegin);
        AddAnchor(rwInherited);

        nextTokenKind := SkipUntilAnchor(ctx);
        while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwGoto, rwBegin, rwInherited])
              or (nextTokenKind.primitiveKind = pkIdentifier)
        do
        begin
            prevCursor := ctx.Cursor;
            CreateStatement(ctx);
            AddAnchor(rwSemiColon);
            nextTokenKind := SkipUntilAnchor(ctx);
            RemoveAnchor(rwSemiColon);
            if PeekReservedWord(ctx, rwSemiColon) then
                TReservedWord.Create(ctx, rwSemiColon, false)
            else if not (nextTokenKind.reservedWordKind in [rwEnd, rwUntil, rwElse, rwOtherwise, rwExcept, rwFinally, rwInitialization, rwFinalization]) then
                TReservedWord.Create(ctx, rwSemiColon, false);
            nextTokenKind := SkipUntilAnchor(ctx);
            EnsureCursorAdvanced(ctx, prevCursor, nextTokenKind);
        end;

        RemoveAnchor(rwWith);
        RemoveAnchor(rwFor);
        RemoveAnchor(rwCase);
        RemoveAnchor(rwIf);
        RemoveAnchor(rwWhile);
        RemoveAnchor(rwRepeat);
        RemoveAnchor(rwGoto);
        RemoveAnchor(rwBegin);
        RemoveAnchor(rwInherited);
    end;

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwElse);
    RemoveAnchor(rwOtherwise);
    RemoveAnchor(pkIdentifier);

    PopCaseStatement;
    TReservedWord.Create(ctx, rwEnd, false);
    ctx.MarkEndOfToken(Self);
end;

end.
