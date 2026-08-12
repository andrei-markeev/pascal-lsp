unit PointerSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef, TypeDefs, Token, ReservedWord;

type
    TPointerSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef); overload;
        constructor Create(ctx: TParserContext; pointerRW: TReservedWord; var typeDefToFill: TTypeDef); overload;
    end;

implementation

uses
    CompilationMode, TypeSpec, PointerTypeDef;

constructor TPointerSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    targetType: TTypeDef;
    ptrTypeDef: TPointerTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'PointerSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwHat, true);
    if mfPointerTo in Features[ctx.mode] then
    begin
        state := tsError;
        errorMessage := 'Use "pointer to" instead of "^"';
    end;

    targetType := unknownType;
    ptrTypeDef := TPointerTypeDef.Create(ctx, true, targetType, 8);
    typeDefToFill := ptrTypeDef;

    CreateTypeSpec(ctx, targetType);
    ptrTypeDef.pointerToType := targetType;

    if state <> tsError then
        state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

constructor TPointerSpec.Create(ctx: TParserContext; pointerRW: TReservedWord; var typeDefToFill: TTypeDef);
var
    targetType: TTypeDef;
    ptrTypeDef: TPointerTypeDef;
begin
    ctx.InsertBefore(pointerRW, Self);
    tokenName := 'PointerSpec';
    start := pointerRW.start;

    TReservedWord.Create(ctx, rwTo, false);

    targetType := unknownType;
    ptrTypeDef := TPointerTypeDef.Create(ctx, true, targetType, 8);
    typeDefToFill := ptrTypeDef;

    CreateTypeSpec(ctx, targetType);
    ptrTypeDef.pointerToType := targetType;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
