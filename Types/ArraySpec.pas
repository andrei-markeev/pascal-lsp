unit ArraySpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TArraySpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, Token, ReservedWord, TypeSpec;

constructor TArraySpec.Create(ctx: TParserContext);
var
    nextIsComma: boolean;
    spec: TTypeSpec;
    curTypeDef: PTypeDef;
    nextTypeDef: PTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'ArraySpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwArray, true);

    typeDef.kind := tkArray;

    if PeekReservedWord(ctx, rwOpenSquareBracket) then
    begin
        TReservedWord.Create(ctx, rwOpenSquareBracket, true);
        curTypeDef := @typeDef;
        curTypeDef^.kind := tkArray;
        repeat
            spec := TTypeSpec.Create(ctx);
            curTypeDef^.typeOfIndex := @spec.typeDef;

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

        spec := TTypeSpec.Create(ctx);
        curTypeDef^.typeOfValues := @spec.typeDef;
    end
    else
    begin

        TReservedWord.Create(ctx, rwOf, false);

        spec := TTypeSpec.Create(ctx);
        typeDef.size := 8; // TODO: detect arch size
        typeDef.kind := tkDynamicArray;
        typeDef.typeOfDynValues := @spec.typeDef;

    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
