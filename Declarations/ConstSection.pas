unit ConstSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, ReservedWord, ConstDecl;

type
    TConstSection = class(TToken)
    public
        decls: array of TConstDecl;
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TConstSection.Create(ctx: TParserContext);
var
    l: integer;
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'ConstSection';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwConst, true);

    repeat
        l := length(decls);
        SetLength(decls, l + 1);
        decls[l] := TConstDecl.Create(ctx);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until nextReservedWord <> rwUnknown;

    ctx.MarkEndOfToken(Self);
end;

end.
