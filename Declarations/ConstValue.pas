unit ConstValue;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, Identifier, Number, StringToken;

type
    TConstValue = class(TToken)
    public
        valueToken: TToken;
        valueType: TTypeKind;
        constructor Create(ctx: TParserContext; tokenKind: TTokenKind);
    end;

implementation

constructor TConstValue.Create(ctx: TParserContext; tokenKind: TTokenKind);
var
    symbol: TSymbol;
begin
    ctx.Add(Self);
    tokenName := 'ConstValue';
    start := ctx.Cursor;
    valueType := tkUnknown;

    // TODO: support turbo pascal constant expressions

    case tokenKind.primitiveKind of
        pkNumber:
            begin
                valueToken := TNumber.Create(ctx);
                if TNumber(valueToken).typeDef.kind = tkInteger then
                    valueType := tkInteger
                else
                    valueType := tkReal;
            end;
        pkString:
            begin
                valueToken := TStringToken.Create(ctx);
                if TStringToken(valueToken).stringLen = 1 then
                    valueType := tkChar
                else
                    valueType := tkString;
            end;
        pkIdentifier:
            begin
                valueToken := TIdentifier.Create(ctx, true);
                symbol := TSymbol(TIdentifier(valueToken).symbol);

                if symbol <> nil then
                begin
                    if symbol.kind <> skConstant then
                    begin
                        state := tsError;
                        errorMessage := 'Only constants can be used when defining other constants!';
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;

                    valueType := symbol.typeDef.kind;
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
