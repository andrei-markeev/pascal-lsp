unit Number;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TNumberKind = (ntInteger, ntReal);
    TNumber = class(TToken)
    public
        kind: TNumberKind;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TNumber.Create(ctx: TParserContext);
var
    numberBase: byte;
begin
    tokenName := 'Num';
    ctx.SkipTrivia;

    start := ctx.Cursor;

    if (ctx.mode >= cmTurboPascal) and (ctx.Cursor[0] = '$') then
        numberBase := 16
    else if (ctx.mode >= cmFreePascal) and (ctx.Cursor[0] = '&') then
        numberBase := 8
    else
        numberBase := 10;

    if numberBase <> 10 then
        inc(ctx.Cursor);

    if (numberBase <> 10) and not (ctx.Cursor[0] in ['0'..'9']) then
    begin
        len := 1;
        state := tsError;
        errorMessage := 'Digit missing after ' + ctx.Cursor[-1];
        ctx.Add(Self);
        exit;
    end;

    if not (ctx.Cursor[0] in ['0'..'9']) then
    begin
        len := 0;
        state := tsMissing;
        ctx.Add(Self);
        exit;
    end;

    while ctx.Cursor[0] in ['0'..'9'] do
        inc(ctx.Cursor);

    if ctx.Cursor[0] = '.' then
    begin
        inc(ctx.Cursor);
        kind := ntReal;
        if not (ctx.Cursor[0] in ['0'..'9']) then
        begin
            state := tsError;
            errorMessage := 'Number expected after .';
        end;
        while ctx.Cursor[0] in ['0'..'9'] do
            inc(ctx.Cursor);
    end
    else
        kind := ntInteger;

    if ctx.Cursor[0] in ['E','e'] then
    begin
        inc(ctx.Cursor);
        kind := ntReal;
        if not (ctx.Cursor[0] in ['0'..'9', '-', '+']) then
        begin
            state := tsError;
            errorMessage := 'Number expected after E';
        end;
        if ctx.Cursor[0] in ['-','+'] then
            inc(ctx.Cursor);
        if not (ctx.Cursor[0] in ['0'..'9']) then
        begin
            state := tsError;
            errorMessage := 'Number expected after E';
        end;
        while ctx.Cursor[0] in ['0'..'9'] do
            inc(ctx.Cursor);
    end;

    len := ctx.Cursor - start;
    ctx.Add(Self);
end;

destructor TNumber.Destroy;
begin
end;

end.
