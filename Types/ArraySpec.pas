unit ArraySpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypeDefs;

type
    TArraySpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    ReservedWord, TypeSpec, ArrayTypeDef, DynamicArrayTypeDef;

constructor TArraySpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    nextIsComma: boolean;
    curTypeDef: TArrayTypeDef;
    nextTypeDef: TArrayTypeDef;
    indexType, valuesType, dynValuesType: TTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'ArraySpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwArray, true);

    if PeekReservedWord(ctx, rwOpenSquareBracket) then
    begin
        TReservedWord.Create(ctx, rwOpenSquareBracket, true);
        curTypeDef := TArrayTypeDef.Create;
        typeDefToFill := curTypeDef;
        repeat
            indexType := unknownType;
            CreateTypeSpec(ctx, indexType);
            curTypeDef.typeOfIndex := indexType;

            nextIsComma := PeekReservedWord(ctx, rwComma);
            if nextIsComma then
            begin
                TReservedWord.Create(ctx, rwComma, true);
                nextTypeDef := TArrayTypeDef.Create;
                curTypeDef.typeOfValues := nextTypeDef;
                curTypeDef := nextTypeDef;
            end;
        until not nextIsComma;
        TReservedWord.Create(ctx, rwCloseSquareBracket, false);

        TReservedWord.Create(ctx, rwOf, false);

        valuesType := unknownType;
        CreateTypeSpec(ctx, valuesType);
        curTypeDef.typeOfValues := valuesType;
    end
    else
    begin

        TReservedWord.Create(ctx, rwOf, false);

        dynValuesType := unknownType;
        typeDefToFill := TDynamicArrayTypeDef.Create(nil, 8);
        CreateTypeSpec(ctx, dynValuesType);
        TDynamicArrayTypeDef(typeDefToFill).typeOfDynValues := dynValuesType;

    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
