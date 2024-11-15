unit FunctionDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Modifiers, Symbols, Token, ReservedWord, TypedToken, TypeDefs, Identifier;

type
    TFunctionDecl = class(TToken)
    public
        nameIdent: TIdentifier;
        paramDecls: TTypedTokenArray;
        funcType: TTypeDef;
        returnType: TTypeDef;
        funcModifiers: TFunctionModifiers;
        methodModifiers: TMethodModifiers;
        constructor Create(ctx: TParserContext; functionRWKind: TReservedWordKind; parentSymbols: array of TSymbol);
    end;

implementation

uses
    Scopes, TypeSpec, ParameterDecl;

constructor TFunctionDecl.Create(ctx: TParserContext; functionRWKind: TReservedWordKind; parentSymbols: array of TSymbol);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType: boolean;
    symbolKind: TSymbolKind;
    paramDecl: TParameterDecl;
    i, p: integer;
    s: string;
    rw: TReservedWord;
    hasMoreParams: boolean;
begin
    ctx.Add(Self);
    tokenName := 'FunctionDecl';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    if not (functionRWKind in [rwFunction, rwProcedure, rwConstructor, rwDestructor]) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    needsReturnType := functionRWKind = rwFunction;

    TReservedWord.Create(ctx, functionRWKind, true);
    case functionRWKind of
        rwFunction:
            begin
                symbolKind := skFunction;
                funcType.kind := tkFunction;
            end;
        rwProcedure:
            begin
                tokenName := 'ProcedureDecl';
                symbolKind := skProcedure;
                funcType.kind := tkProcedure;
            end;
        rwConstructor:
            begin
                tokenName := 'ConstructorDecl';
                symbolKind := skConstructor;
                funcType.kind := tkFunction;
            end;
        rwDestructor:
            begin
                tokenName := 'DestructorDecl';
                symbolKind := skDestructor;
                funcType.kind := tkProcedure;
            end;
    end;

    nameIdent := TIdentifier.Create(ctx, false);
    if FindSymbol(nameIdent) <> nil then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Duplicate identifier!';
    end;

    SetString(s, nameIdent.start, nameIdent.len);
    if (nameIdent.state <> tsError) and (symbolKind = skDestructor) and (LowerCase(s) <> 'destroy') then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Destructor must be called ''Destroy''!';
    end;

    paramDecls := TTypedTokenArray.Create;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if nextReservedWordKind = rwOpenParenthesis then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);

        hasMoreParams := false;
        repeat
            paramDecl := TParameterDecl.Create(ctx);
            for i := 0 to length(paramDecl.idents) - 1 do
                paramDecls.Add(paramDecl);

            if PeekReservedWord(ctx, rwComma) then
            begin
                // common error, mixing up ";" and ","
                hasMoreParams := true;
                rw := TReservedWord.Create(ctx, rwComma, true);
                rw.state := tsSkipped;
                TReservedWord.Create(ctx, rwSemiColon, false);
            end
            else
            begin
                hasMoreParams := PeekReservedWord(ctx, rwSemiColon);
                if hasMoreParams then
                    TReservedWord.Create(ctx, rwSemiColon, true);
            end;
        until hasMoreParams = false;

        TReservedWord.Create(ctx, rwCloseParenthesis, false);
    end;

    funcType.parameters := paramDecls;

    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        returnType := CreateTypeSpec(ctx).typeDef;
    end
    else if (symbolKind = skConstructor) and (length(parentSymbols) > 0) then
        returnType := parentSymbols[0].typeDef;

    funcType.returnType := @returnType;

    for p := 0 to length(parentSymbols) - 1 do
        RegisterSymbol(nameIdent, parentSymbols[p], symbolKind, funcType, ctx.Cursor);

    TReservedWord.Create(ctx, rwSemiColon, false);

    repeat
        ctx.SkipTrivia;
        s := PeekIdentifier(ctx);
        case LowerCase(s) of
            'override': methodModifiers.override := true;
            'reintroduce': methodModifiers.reintroduce := true;
            'virtual': methodModifiers.virtual := true;
            // TODO: message
            // TODO: funcModifiers
        else
            break;
        end;
        TIdentifier.Create(ctx, false);
        TReservedWord.Create(ctx, rwSemiColon, false);
    until ctx.IsEOF;

    if (nameIdent.state = tsCorrect) and (symbolKind = skDestructor) and not methodModifiers.override then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Destructor must have ''override'' modifier!';
    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
