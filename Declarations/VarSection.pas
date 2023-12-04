unit VarSection;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, ReservedWord,
    VarDecl in 'Declarations/VarDecl.pas';

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

    repeat
        l := length(decls);
        SetLength(decls, l + 1);
        decls[l] := TVarDecl.Create(ctx);

        TReservedWord.Create(ctx, rwSemicolon, false);
        nextReservedWord := DetermineReservedWord(ctx);
    until nextReservedWord <> rwUnknown;

    ctx.MarkEndOfToken(Self);
end;

destructor TVarSection.Destroy;
begin
end;

end.
