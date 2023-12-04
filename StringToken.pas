unit StringToken;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TStringToken = class(TToken)
    public
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TStringToken.Create(ctx: TParserContext);
begin
    tokenName := 'Str';
    isPrimitive := true;
    ctx.SkipTrivia;

    start := ctx.Cursor;
    if not (ctx.Cursor[0] in ['''', '#']) then
    begin
        len := 0;
        state := tsMissing;
        ctx.Add(Self);
        exit;
    end;

    repeat

        if ctx.Cursor[0] = '#' then
        begin
            inc(ctx.Cursor);
            while ctx.Cursor[0] in ['0'..'9'] do
                inc(ctx.Cursor);

            len := ctx.Cursor - start;
            if len < 2 then
            begin
                state := tsError;
                exit;
            end;
        end
        else
        begin

            repeat
                inc(ctx.Cursor);
            until ctx.Cursor[0] in ['''', #10, #13, #0];

            if ctx.Cursor[0] <> '''' then
            begin
                len := ctx.Cursor - start;
                state := tsError;
                exit;
            end;

            inc(ctx.Cursor);
        end;

    until not (ctx.Cursor[0] in ['''', '#']);

    len := ctx.Cursor - start;
    state := tsCorrect;

    ctx.Add(Self);
end;

destructor TStringToken.Destroy;
begin
end;

end.
