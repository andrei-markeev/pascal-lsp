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
    Anchors, TypeDefs, Token, ReservedWord, TypeSpec;

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
        repeat
            spec := TTypeSpec.Create(ctx);
            curTypeDef := new(PTypeDef);
            curTypeDef^.kind := tkArray;
            curTypeDef^.typeOfIndex := @spec.typeDef;

            nextIsComma := PeekReservedWord(ctx, rwComma);
            if nextIsComma then
            begin
                TReservedWord.Create(ctx, rwComma, true);
                nextTypeDef := new(PTypeDef);
                nextTypeDef^.kind := tkArray;
                curTypeDef^.typeOfValues := nextTypeDef;
                nextTypeDef := curTypeDef;
            end;
        until not nextIsComma;
        TReservedWord.Create(ctx, rwCloseSquareBracket, false);

        TReservedWord.Create(ctx, rwOf, false);

        spec := TTypeSpec.Create(ctx);
        curTypeDef^.typeOfValues := @spec.typeDef;
        typeDef := curTypeDef^;
    end
    else
    begin

        TReservedWord.Create(ctx, rwOf, false);

        spec := TTypeSpec.Create(ctx);
        typeDef.kind := tkDynamicArray;
        typeDef.typeOfDynValues := @spec.typeDef;

    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
