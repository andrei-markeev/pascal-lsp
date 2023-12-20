unit SetSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TSetSpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, Token, ReservedWord, TypeSpec;

constructor TSetSpec.Create(ctx: TParserContext);
var
    spec: TTypeSpec;
begin
    ctx.Add(Self);
    tokenName := 'SetSpec';
    start := ctx.Cursor;
    
    TReservedWord.Create(ctx, rwSet, true);
    TReservedWord.Create(ctx, rwOf, false);

    spec := TTypeSpec.Create(ctx);
    typeDef.size := 1;
    typeDef.kind := tkSet;
    typeDef.typeOfSet := @spec.typeDef;

    if not (spec.typeDef.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        errorMessage := 'Expected set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(spec.typeDef.kind)];
    end
    else if spec.typeDef.size > 1 then
    begin
        state := tsError;
        errorMessage := 'The base type of the set must not have more than 256 possible values!';
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
