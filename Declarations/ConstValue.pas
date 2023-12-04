unit ConstValue;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, Identifier, Number, StringToken;

type
    TConstValue = class(TToken)
    public
        valueToken: TToken;
        valueType: TPrimitiveKind;
        constructor Create(ctx: TParserContext; tokenKind: TTokenKind);
        destructor Destroy; override;
    end;

implementation

constructor TConstValue.Create(ctx: TParserContext; tokenKind: TTokenKind);
begin
    ctx.Add(Self);
    tokenName := 'ConstValue';
    start := ctx.Cursor;
    state := tsCorrect;

    // todo: support cmTurboPascal and up constant expressions

    case tokenKind.primitiveKind of
        pkNumber: valueToken := TNumber.Create(ctx);
        pkString: valueToken := TStringToken.Create(ctx);
        pkIdentifier: valueToken := TIdentifier.Create(ctx);
    else
        start := ctx.cursorBeforeTrivia;
        state := tsMissing;
        len := 0;
        exit;
    end;

    valueType := tokenKind.primitiveKind;
    ctx.MarkEndOfToken(Self);
end;

destructor TConstValue.Destroy;
begin
end;

end.
