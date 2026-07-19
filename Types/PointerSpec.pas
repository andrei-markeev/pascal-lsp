unit PointerSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Token;

type
    TPointerSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    ReservedWord, TypeSpec, PointerTypeDef;

constructor TPointerSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    targetType: TTypeDef;
    ptrTypeDef: TPointerTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'PointerSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwHat, true);

    targetType := unknownType;
    ptrTypeDef := TPointerTypeDef.Create(ctx, true, targetType, 8);
    typeDefToFill := ptrTypeDef;

    CreateTypeSpec(ctx, targetType);
    ptrTypeDef.pointerToType := targetType;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
