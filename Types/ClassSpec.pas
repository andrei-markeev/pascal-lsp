unit ClassSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, TypedToken;

type
    TClassSpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol);
    end;

implementation

uses
    Anchors, Token, ReservedWord, TypeSpec, VarDecl, FunctionDecl;

constructor TClassSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    i: integer;
    fieldDecl: TVarDecl;
    funcDecl: TFunctionDecl;
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'ClassSpec';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDef.size := 0;
    typeDef.kind := tkClass;
    typeDef.fields := TFPHashList.Create; // TODO: free memory

    // TODO: packed classes

    TReservedWord.Create(ctx, rwClass, true);

    // TODO: abstract, sealed
    // TODO: heritage
    // TODO: visibility

    nextTokenKind := DetermineNextTokenKind(ctx);

    while (nextTokenKind.primitiveKind = pkIdentifier) or (nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor]) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            fieldDecl := TVarDecl.Create(ctx, parentSymbols);
            for i := 0 to length(fieldDecl.idents) - 1 do
            begin
                typeDef.fields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType.typeDef);
                inc(typeDef.size, fieldDecl.varType.typeDef.size);
            end;
            TReservedWord.Create(ctx, rwSemiColon, false);

            // TODO: static modifier
        end
        else if nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor] then
        begin
            funcDecl := TFunctionDecl.Create(ctx, nextTokenKind.reservedWordKind, parentSymbols);
            typeDef.fields.Add(funcDecl.nameIdent.GetStr(), @funcDecl.funcType);
            // TODO: modifiers
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    TReservedWord.Create(ctx, rwEnd, true);

    ctx.MarkEndOfToken(Self);
end;

end.
