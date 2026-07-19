unit ConstValue;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypedToken;

type
    TConstValue = class(TTypedToken)
    public
        valueToken: TTypedToken;
        constructor Create(ctx: TParserContext; tokenKind: TTokenKind);
    end;

implementation

uses
    CompilationMode, Symbols, TypeDefs, Token, Identifier, Number, StringToken, ReservedWord, ArrayLiteral;

constructor TConstValue.Create(ctx: TParserContext; tokenKind: TTokenKind);
var
    symbol: TSymbol;
begin
    ctx.Add(Self);
    tokenName := 'ConstValue';
    start := ctx.Cursor;
    typeDef := unknownType;

    if (ctx.mode >= cmTurboPascal) and (tokenKind.reservedWordKind = rwOpenParenthesis) then
    begin
        valueToken := TArrayLiteral.Create(ctx);
        if valueToken <> nil then
            typeDef := valueToken.typeDef;
    end
    else
    case tokenKind.primitiveKind of
        pkNumber:
            begin
                valueToken := TNumber.Create(ctx);
                if valueToken <> nil then
                    typeDef := valueToken.typeDef;
            end;
        pkString:
            begin
                valueToken := TStringToken.Create(ctx);
                if valueToken <> nil then
                    typeDef := valueToken.typeDef;
            end;
        pkIdentifier:
            begin
                valueToken := TIdentifier.Create(ctx, true);
                if valueToken <> nil then
                    typeDef := valueToken.typeDef;

                symbol := TSymbol(TIdentifier(valueToken).symbol);
                if (symbol <> nil) and (symbol.kind <> skConstant) then
                begin
                    state := tsError;
                    errorMessage := 'Only constants can be used when defining other constants!';
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;
            end
    else
        start := ctx.cursorBeforeTrivia;
        state := tsMissing;
        len := 0;
        exit;
    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
