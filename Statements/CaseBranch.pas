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
    Number, StringToken, Identifier, Statement;

procedure ParseCaseConstant(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
begin
    nextTokenKind := DetermineNextTokenKind(ctx);
    case nextTokenKind.primitiveKind of
        pkNumber: TNumber.Create(ctx);
        pkString: TStringToken.Create(ctx);
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
begin
    ctx.Add(Self);
    tokenName := 'CaseBranch';
    start := ctx.Cursor;

    ParseCaseConstant(ctx);

    if PeekReservedWord(ctx, rwRange) then
    begin
        TReservedWord.Create(ctx, rwRange, true);
        ParseCaseConstant(ctx);
    end;

    while PeekReservedWord(ctx, rwComma) do
    begin
        TReservedWord.Create(ctx, rwComma, true);
        ParseCaseConstant(ctx);
        if PeekReservedWord(ctx, rwRange) then
        begin
            TReservedWord.Create(ctx, rwRange, true);
            ParseCaseConstant(ctx);
        end;
    end;

    TReservedWord.Create(ctx, rwColon, false);
    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
