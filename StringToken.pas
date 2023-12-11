unit StringToken;

{$mode objfpc}
{$longstrings on}

interface

uses
    CompilationMode, TypeDefs, ParserContext, Token, TypedToken;

type
    TStringToken = class(TTypedToken)
    public
        stringLen: integer;
        charValue: char;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TStringToken.Create(ctx: TParserContext);
begin
    tokenName := 'Str';
    isPrimitive := true;
    ctx.SkipTrivia;
    stringLen := 0;
    charValue := #0;

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

            if (ctx.mode >= cmTurboPascal) and (ctx.Cursor[0] = '$') then
            begin
                inc(ctx.Cursor);
                while ctx.Cursor[0] in ['0'..'9', 'a'..'f', 'A'..'F'] do
                    inc(ctx.Cursor);
            end
            else if (ctx.mode >= cmFreePascal) and (ctx.Cursor[0] = '&') then
            begin
                inc(ctx.Cursor);
                while ctx.Cursor[0] in ['0'..'7'] do
                    inc(ctx.Cursor);
            end
            else
            begin
                while ctx.Cursor[0] in ['0'..'9'] do
                    inc(ctx.Cursor);
            end;

            len := ctx.Cursor - start;
            if len < 2 then
            begin
                state := tsError;
                exit;
            end;

            inc(stringLen);
        end
        else
        begin

            repeat
                inc(ctx.Cursor);
                inc(stringLen);
            until ctx.Cursor[0] in ['''', #10, #13, #0];

            dec(stringLen); // compensate for the opening '

            if ctx.Cursor[0] <> '''' then
            begin
                len := ctx.Cursor - start;
                state := tsError;
                exit;
            end;

            inc(ctx.Cursor);
            if ctx.Cursor[0] = '''' then inc(stringLen); // double '
        end;

    until not (ctx.Cursor[0] in ['''', '#']);

    len := ctx.Cursor - start;
    state := tsCorrect;
    if stringLen = 1 then
        typeDef := charType
    else
    begin
        typeDef.size := stringLen;
        typeDef.kind := tkString;
    end;

    ctx.Add(Self);
end;

destructor TStringToken.Destroy;
begin
end;

end.
