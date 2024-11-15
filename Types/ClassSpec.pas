unit ClassSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, Token;

type
    TClassSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    CompilationMode, Anchors, ReservedWord, Identifier, VarDecl, FunctionDecl;

procedure SetVisibility(ctx: TParserContext; const value: TVisibility; out res: TVisibility);
begin
    res := value;
    TIdentifier.Create(ctx, false);
end;

constructor TClassSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
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
    typeDefToFill.size := 0;
    typeDefToFill.kind := tkClass;
    typeDefToFill.fields := TFPHashList.Create; // TODO: free memory
    typeDefToFill.fields.Add('Free', @voidProcedureType);

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
                    typeDefToFill.fields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType);
                    fieldDecl.varType.visibility := visibility;
                    inc(typeDefToFill.size, fieldDecl.varType.size);
                end;
                TReservedWord.Create(ctx, rwSemiColon, false);
            end;

            // TODO: static modifier
        end
        else if nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor] then
        begin
            funcDecl := TFunctionDecl.Create(ctx, nextTokenKind.reservedWordKind, parentSymbols);
            funcDecl.funcType.visibility := visibility;
            typeDefToFill.fields.Add(funcDecl.nameIdent.GetStr(), @funcDecl.funcType);
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    TReservedWord.Create(ctx, rwEnd, true);

    ctx.MarkEndOfToken(Self);
end;

end.
