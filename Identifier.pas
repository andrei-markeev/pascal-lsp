unit Identifier;

{$mode objfpc}
{$longstrings on}

interface

uses
    math, ParserContext, Token, TypedToken;

type
    TIdentifier = class(TTypedToken)
    public
        symbol: pointer;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
        function GetName: shortstring;
    end;

function PeekIdentifier(ctx: TParserContext): shortstring;

implementation

function PeekIdentifier(ctx: TParserContext): shortstring;
var
    cursor: PChar;
    len: integer;
begin
    cursor := ctx.Cursor;

    if cursor[0] in ['a'..'z', 'A'..'Z', '_'] then
    begin
        inc(cursor);
        while cursor[0] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
            inc(cursor);

        len := Cursor - ctx.Cursor;

        SetString(PeekIdentifier, ctx.Cursor, Min(255, len));
    end
    else
        PeekIdentifier := ''
end;

constructor TIdentifier.Create(ctx: TParserContext);
begin
    tokenName := 'Ident';
    isPrimitive := true;
    ctx.SkipTrivia;
    if ctx.Cursor[0] in ['a'..'z', 'A'..'Z', '_'] then
    begin
        start := ctx.Cursor;
        inc(ctx.Cursor);
        while ctx.Cursor[0] in ['a'..'z', 'A'..'Z', '_', '0'..'9'] do
            inc(ctx.Cursor);
        state := tsCorrect;
        len := ctx.Cursor - start;
    end
    else
    begin
        start := ctx.Cursor;
        len := 0;
        state := tsMissing;
    end;

    ctx.Add(Self);
end;

destructor TIdentifier.Destroy;
begin
end;

function TIdentifier.GetName: shortstring;
begin
    SetString(GetName, start, len);
end;

end.
