unit Number;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TNumber = class(TTypedToken)
    private
        procedure ParseHexNumber(ctx: TParserContext);
        procedure ParseDecNumber(ctx: TParserContext);
        procedure ParseOctNumber(ctx: TParserContext);
    public
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

uses
    CompilationMode, Token, TypeDefs;

procedure TNumber.ParseHexNumber(ctx: TParserContext);
begin
    inc(ctx.Cursor); // '$'

    if not (ctx.Cursor[0] in ['0'..'9', 'a'..'f', 'A'..'F']) then
    begin
        len := 1;
        state := tsError;
        errorMessage := 'Digit missing after ' + ctx.Cursor[-1];
        exit;
    end;

    while ctx.Cursor[0] in ['0'..'9', 'a'..'f', 'A'..'F'] do
        inc(ctx.Cursor);

    len := ctx.Cursor - start;
end;

procedure TNumber.ParseOctNumber(ctx: TParserContext);
begin
    inc(ctx.Cursor); // '&'

    if not (ctx.Cursor[0] in ['0'..'7']) then
    begin
        len := 1;
        state := tsError;
        errorMessage := 'Digit missing after ' + ctx.Cursor[-1];
        exit;
    end;

    while ctx.Cursor[0] in ['0'..'7'] do
        inc(ctx.Cursor);

    len := ctx.Cursor - start;
end;

procedure TNumber.ParseDecNumber(ctx: TParserContext);
begin
    if not (ctx.Cursor[0] in ['0'..'9']) then
    begin
        len := 0;
        state := tsMissing;
        exit;
    end;

    while ctx.Cursor[0] in ['0'..'9'] do
        inc(ctx.Cursor);

    if ctx.Cursor[0] = '.' then
    begin
        if ctx.Cursor[1] = '.' then
        begin
            len := ctx.Cursor - start;
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
end;

constructor TNumber.Create(ctx: TParserContext);
begin
    tokenName := 'Num';
    ctx.Add(Self);

    isPrimitive := true;
    ctx.SkipTrivia;

    start := ctx.Cursor;
    typeDef := smallintType;

    if (ctx.mode >= cmTurboPascal) and (ctx.Cursor[0] = '$') then
        ParseHexNumber(ctx)
    else if (ctx.mode >= cmFreePascal) and (ctx.Cursor[0] = '&') then
        ParseOctNumber(ctx)
    else
        ParseDecNumber(ctx);

end;

destructor TNumber.Destroy;
begin
end;

end.
