unit TypeSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, Anchors, ReservedWord, TypeDecl;

type
    TTypeSection = class(TToken)
    public
        decls: array of TTypeDecl;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TTypeSection.Create(ctx: TParserContext);
var
    l: integer;
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'TypeSection';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwType, true);

    AddAnchor(rwSemiColon);
    repeat

        l := length(decls);
        SetLength(decls, l + 1);
        decls[l] := TTypeDecl.Create(ctx);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until ctx.IsEOF or (nextReservedWord <> rwUnknown);
    RemoveAnchor(rwSemiColon);

    ctx.MarkEndOfToken(Self);
end;

destructor TTypeSection.Destroy;
begin
end;

end.
