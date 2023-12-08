unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, Token, ReservedWord, Statement;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

function CreateBlock(ctx: TParserContext): TToken;

implementation

function CreateBlock(ctx: TParserContext): TToken;
begin
    CreateBlock := TBlock.Create(ctx);
end;

constructor TBlock.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwBegin, false);

    AddAnchor(rwEnd);
    AddAnchor(rwWith);
    AddAnchor(rwFor);
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwGoto);
    AddAnchor(rwBegin);
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
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
