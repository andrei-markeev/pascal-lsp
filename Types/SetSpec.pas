unit SetSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Token;

type
    TSetSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    TypeDef, ReservedWord, TypeSpec, SetTypeDef;

constructor TSetSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    baseType: TTypeDef;
    setTypeDef: TSetTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'SetSpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwSet, true);
    TReservedWord.Create(ctx, rwOf, false);

    baseType := unknownType;
    setTypeDef := TSetTypeDef.Create(ctx, baseType, 1);
    typeDefToFill := setTypeDef;

    CreateTypeSpec(ctx, baseType);
    setTypeDef.typeOfSet := baseType;

    if (baseType = nil) or not (baseType.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        if (baseType <> nil) and (ord(baseType.kind) >= 0) and (ord(baseType.kind) < NUM_OF_TYPE_KINDS) then
            errorMessage := 'Expected set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(baseType.kind)]
        else
            errorMessage := 'Expected set of ordinal type.';
    end
    else if baseType.size > 1 then
    begin
        state := tsError;
        errorMessage := 'The base type of the set must not have more than 256 possible values!';
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
