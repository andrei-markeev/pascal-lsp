unit VarSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, Anchors, ReservedWord, VarDecl;

type
    TVarSection = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TVarSection.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'VarSection';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwVar, true);

    AddAnchor(rwSemiColon);
    repeat

        TVarDecl.Create(ctx, [nil]);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until ctx.IsEOF or (nextReservedWord <> rwUnknown);
    RemoveAnchor(rwSemiColon);

    ctx.MarkEndOfToken(Self);
end;

end.
