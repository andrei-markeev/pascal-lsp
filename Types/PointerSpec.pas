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
    ReservedWord, TypeSpec;

constructor TPointerSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
begin
    ctx.Add(Self);
    tokenName := 'PointerSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwHat, true);

    typeDefToFill.kind := tkPointer;
    typeDefToFill.size := 8; // TODO: detect arch size
    typeDefToFill.isTyped := true;
    typeDefToFill.pointerToType := new(PTypeDef); // TODO: free memory
    CreateTypeSpec(ctx, typeDefToFill.pointerToType^);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
