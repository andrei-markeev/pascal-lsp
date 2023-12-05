unit VarSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, Anchors, ReservedWord, VarDecl;

type
    TVarSection = class(TToken)
    public
        decls: array of TVarDecl;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TVarSection.Create(ctx: TParserContext);
var
    l: integer;
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'VarSection';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwVar, true);

    AddAnchor(rwSemiColon);
    repeat

        l := length(decls);
        SetLength(decls, l + 1);
        decls[l] := TVarDecl.Create(ctx);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until ctx.IsEOF or (nextReservedWord <> rwUnknown);
    RemoveAnchor(rwSemiColon);

    ctx.MarkEndOfToken(Self);
end;

destructor TVarSection.Destroy;
begin
end;

end.
