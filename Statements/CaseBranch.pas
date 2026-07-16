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

implementation

uses
    CompilationMode, Number, StringToken, Identifier, Statement;

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
            if ctx.mode < cmFreePascal then
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
begin
    ctx.Add(Self);
    tokenName := 'CaseBranch';
    start := ctx.Cursor;

    ParseCaseConstant(ctx);

    if PeekReservedWord(ctx, rwRange) then
    begin
        rangeRW := TReservedWord.Create(ctx, rwRange, true);
        if ctx.mode = cmStandardPascal then
        begin
            rangeRW.state := tsError;
            rangeRW.errorMessage := '".." ranges in case statements not supported in Standard Pascal (ISO 7185)';
        end;
        ParseCaseConstant(ctx);
    end;

    while PeekReservedWord(ctx, rwComma) do
    begin
        TReservedWord.Create(ctx, rwComma, true);
        ParseCaseConstant(ctx);
        if PeekReservedWord(ctx, rwRange) then
        begin
            rangeRW := TReservedWord.Create(ctx, rwRange, true);
            if ctx.mode = cmStandardPascal then
            begin
                rangeRW.state := tsError;
                rangeRW.errorMessage := '".." ranges in case statements not supported in Standard Pascal (ISO 7185)';
            end;
            ParseCaseConstant(ctx);
        end;
    end;

    TReservedWord.Create(ctx, rwColon, false);
    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
