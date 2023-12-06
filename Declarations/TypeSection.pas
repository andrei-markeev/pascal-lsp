unit TypeSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, Anchors, ReservedWord, TypeDecl;

type
    TTypeSection = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TTypeSection.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'TypeSection';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwType, true);

    AddAnchor(rwSemiColon);
    repeat

        TTypeDecl.Create(ctx);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until ctx.IsEOF or (nextReservedWord <> rwUnknown);
    RemoveAnchor(rwSemiColon);

    ctx.MarkEndOfToken(Self);
end;

end.
