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
    ReservedWord, TypeSpec;

constructor TArraySpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    nextIsComma: boolean;
    curTypeDef: PTypeDef;
    nextTypeDef: PTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'ArraySpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwArray, true);

    typeDefToFill.kind := tkArray;

    if PeekReservedWord(ctx, rwOpenSquareBracket) then
    begin
        TReservedWord.Create(ctx, rwOpenSquareBracket, true);
        curTypeDef := @typeDefToFill;
        curTypeDef^.kind := tkArray;
        repeat
            curTypeDef^.typeOfIndex := new(PTypeDef); // TODO: free memory
            CreateTypeSpec(ctx, curTypeDef^.typeOfIndex^);

            nextIsComma := PeekReservedWord(ctx, rwComma);
            if nextIsComma then
            begin
                TReservedWord.Create(ctx, rwComma, true);
                nextTypeDef := new(PTypeDef); // TODO: free memory
                nextTypeDef^.kind := tkArray;
                curTypeDef^.typeOfValues := nextTypeDef;
                curTypeDef := nextTypeDef;
            end;
        until not nextIsComma;
        TReservedWord.Create(ctx, rwCloseSquareBracket, false);

        TReservedWord.Create(ctx, rwOf, false);

        curTypeDef^.typeOfValues := new(PTypeDef); // TODO: free memory
        CreateTypeSpec(ctx, curTypeDef^.typeOfValues^);
    end
    else
    begin

        TReservedWord.Create(ctx, rwOf, false);

        typeDefToFill.size := 8; // TODO: detect arch size
        typeDefToFill.kind := tkDynamicArray;
        typeDefToFill.typeOfDynValues := new(PTypeDef);
        CreateTypeSpec(ctx, typeDefToFill.typeOfDynValues^);

    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
