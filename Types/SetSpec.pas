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
    ReservedWord, TypeSpec;

constructor TSetSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
begin
    ctx.Add(Self);
    tokenName := 'SetSpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwSet, true);
    TReservedWord.Create(ctx, rwOf, false);

    typeDefToFill.size := 1;
    typeDefToFill.kind := tkSet;
    typeDefToFill.typeOfSet := new(PTypeDef); // TODO: free memory
    CreateTypeSpec(ctx, typeDefToFill.typeOfSet^);

    if not (typeDefToFill.typeOfSet^.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        errorMessage := 'Expected set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(typeDefToFill.typeOfSet^.kind)];
    end
    else if typeDefToFill.typeOfSet^.size > 1 then
    begin
        state := tsError;
        errorMessage := 'The base type of the set must not have more than 256 possible values!';
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
