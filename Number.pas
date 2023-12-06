unit Number;

{$mode objfpc}
{$longstrings on}

interface

uses
    CompilationMode, TypeDefs, ParserContext, Token, TypedToken;

type
    TNumber = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TNumber.Create(ctx: TParserContext);
var
    numberBase: byte;
begin
    tokenName := 'Num';
    isPrimitive := true;
    ctx.SkipTrivia;

    start := ctx.Cursor;
    typeDef := smallintType;

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
        if ctx.Cursor[1] = '.' then
        begin
            len := ctx.Cursor - start;
            ctx.Add(Self);
            exit;
        end;

        inc(ctx.Cursor);
        typeDef := realType;
        if not (ctx.Cursor[0] in ['0'..'9']) then
        begin
            state := tsError;
            errorMessage := 'Number expected after .';
        end;
        while ctx.Cursor[0] in ['0'..'9'] do
            inc(ctx.Cursor);
    end;

    if ctx.Cursor[0] in ['E','e'] then
    begin
        inc(ctx.Cursor);
        typeDef := realType;
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
