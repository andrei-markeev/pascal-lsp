unit CaseBranch;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord;

type
    TCaseBranch = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

procedure ParseCaseConstant(ctx: TParserContext);

implementation

uses
    CompilationMode, Number, StringToken, Identifier, Statement, BranchTracker;

procedure ParseCaseConstant(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    stringTok: TToken;
begin
    nextTokenKind := DetermineNextTokenKind(ctx);
    case nextTokenKind.primitiveKind of
        pkNumber: TNumber.Create(ctx);
        pkString:
        begin
            stringTok := TStringToken.Create(ctx);
            if not (mfStringCaseLabels in Features[ctx.mode]) then
            begin
                stringTok.state := tsError;
                stringTok.errorMessage := 'String case labels not supported in this compilation mode';
            end;
        end;
        pkIdentifier: TIdentifier.Create(ctx, false);
    else
        if PeekReservedWord(ctx, rwMinus) then
        begin
            TReservedWord.Create(ctx, rwMinus, true);
            TNumber.Create(ctx);
        end;
    end;
end;

constructor TCaseBranch.Create(ctx: TParserContext);
var
    rangeRW: TReservedWord;
    cursor1, cursor2, cursor3: PChar;
begin
    ctx.Add(Self);
    tokenName := 'CaseBranch';
    start := ctx.Cursor;

    ClearCurrentBranchLabels;

    repeat
        if (ctx.Cursor > start) and PeekReservedWord(ctx, rwComma) then
            TReservedWord.Create(ctx, rwComma, true);

        ctx.SkipTrivia;
        cursor1 := ctx.Cursor;
        ParseCaseConstant(ctx);
        cursor2 := ctx.Cursor;

        if PeekReservedWord(ctx, rwRange) then
        begin
            rangeRW := TReservedWord.Create(ctx, rwRange, true);
            if not (mfCaseRanges in Features[ctx.mode]) then
            begin
                rangeRW.state := tsError;
                rangeRW.errorMessage := '".." ranges in case statements not supported in Standard Pascal (ISO 7185)';
            end;
            ctx.SkipTrivia;
            cursor3 := ctx.Cursor;
            ParseCaseConstant(ctx);
            AddRangeLabel(cursor1, cursor2 - cursor1, cursor3, ctx.Cursor - cursor3);
        end
        else
            AddSingleLabel(cursor1, cursor2 - cursor1);

    until not PeekReservedWord(ctx, rwComma);

    TReservedWord.Create(ctx, rwColon, false);
    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
