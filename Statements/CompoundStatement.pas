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
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwGoto);
    AddAnchor(rwBegin);
    AddAnchor(rwInitialization);
    AddAnchor(rwFinalization);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwIf, rwWhile, rwRepeat, rwGoto, rwBegin])
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

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwWith);
    RemoveAnchor(rwFor);
    RemoveAnchor(rwIf);
    RemoveAnchor(rwWhile);
    RemoveAnchor(rwRepeat);
    RemoveAnchor(rwGoto);
    RemoveAnchor(rwBegin);
    RemoveAnchor(rwInitialization);
    RemoveAnchor(rwFinalization);
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
