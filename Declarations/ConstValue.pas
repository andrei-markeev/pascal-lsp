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
    Symbols, TypeDefs, Token, Identifier, Number, StringToken;

constructor TConstValue.Create(ctx: TParserContext; tokenKind: TTokenKind);
var
    symbol: TSymbol;
begin
    ctx.Add(Self);
    tokenName := 'ConstValue';
    start := ctx.Cursor;
    typeDef := default(TTypeDef);

    // TODO: support turbo pascal constant expressions

    case tokenKind.primitiveKind of
        pkNumber:
            begin
                valueToken := TNumber.Create(ctx);
                typeDef := valueToken.typeDef;
            end;
        pkString:
            begin
                valueToken := TStringToken.Create(ctx);
                typeDef := valueToken.typeDef;
            end;
        pkIdentifier:
            begin
                valueToken := TIdentifier.Create(ctx, true);
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
