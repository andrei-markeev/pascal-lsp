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
    CompilationMode, Anchors, Token, ReservedWord, TypeSpec, Identifier, VarDecl, FunctionDecl;

procedure SetVisibility(ctx: TParserContext; const value: TVisibility; out res: TVisibility);
begin
    res := value;
    TIdentifier.Create(ctx, false);
end;

constructor TClassSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    i: integer;
    fieldDecl: TVarDecl;
    funcDecl: TFunctionDecl;
    nextTokenKind: TTokenKind;
    s: string;
    visibility: TVisibility;
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

    visibility := vPublic;
    nextTokenKind := DetermineNextTokenKind(ctx);

    while (nextTokenKind.primitiveKind = pkIdentifier) or (nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor]) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            s := LowerCase(PeekIdentifier(ctx));
            case s of
                'private': SetVisibility(ctx, vPrivate, visibility);
                'public': SetVisibility(ctx, vPublic, visibility);
                'protected': if ctx.mode >= cmObjectFreePascal then SetVisibility(ctx, vProtected, visibility);
            else
                fieldDecl := TVarDecl.Create(ctx, parentSymbols);
                for i := 0 to length(fieldDecl.idents) - 1 do
                begin
                    typeDef.fields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType.typeDef);
                    fieldDecl.varType.typeDef.visibility := visibility;
                    inc(typeDef.size, fieldDecl.varType.typeDef.size);
                end;
                TReservedWord.Create(ctx, rwSemiColon, false);
            end;

            // TODO: static modifier
        end
        else if nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor] then
        begin
            funcDecl := TFunctionDecl.Create(ctx, nextTokenKind.reservedWordKind, parentSymbols);
            funcDecl.funcType.visibility := visibility;
            typeDef.fields.Add(funcDecl.nameIdent.GetStr(), @funcDecl.funcType);
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    TReservedWord.Create(ctx, rwEnd, true);

    ctx.MarkEndOfToken(Self);
end;

end.
