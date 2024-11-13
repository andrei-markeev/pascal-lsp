unit FunctionImpl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypedToken, TypeDefs, Identifier;

type
    TFunctionImpl = class(TToken)
    public
        typeIdent: TIdentifier;
        nameIdent: TIdentifier;
        paramDecls: TTypedTokenArray;
        funcType: TTypeDef;
        returnType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, ReservedWord, Scopes, Symbols, TypeSpec, ParameterDecl, Block;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType: boolean;
    symbolKind: TSymbolKind;
    symbolParent: TSymbol;
    paramDecl: TParameterDecl;
    i: integer;
    rw: TReservedWord;
    hasMoreParams: boolean;
begin
    ctx.Add(Self);
    tokenName := 'Function';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if not (nextReservedWordKind in [rwFunction, rwProcedure]) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    needsReturnType := nextReservedWordKind = rwFunction;
    if nextReservedWordKind = rwFunction then
    begin
        symbolKind := skFunction;
        funcType.kind := tkFunction;
        TReservedWord.Create(ctx, rwFunction, true)
    end
    else
    begin
        tokenName := 'Procedure';
        symbolKind := skProcedure;
        funcType.kind := tkProcedure;
        TReservedWord.Create(ctx, rwProcedure, true);
    end;

    nameIdent := TIdentifier.Create(ctx, false);
    typeIdent := nil;
    symbolParent := FindSymbol(nameIdent);
    if (symbolParent <> nil) and (symbolParent.kind = skTypeName) then
    begin
        typeIdent := nameIdent;
        symbolParent.AddReference(typeIdent);
        if PeekReservedWord(ctx, rwDot) then
        begin
            TReservedWord.Create(ctx, rwDot, true);
            nameIdent := TIdentifier.Create(ctx, false);
        end
        else
        begin
            typeIdent.state := tsError;
            typeIdent.errorMessage := 'Previously declared type identifier is used as a ' + LowerCase(tokenName) + ' name!';
        end;
    end
    else if symbolParent <> nil then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Duplicate identifier!';
        symbolParent := nil;
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
    end;

    funcType.returnType := @returnType;

    RegisterSymbol(nameIdent, symbolParent, symbolKind, funcType, ctx.Cursor);

    // TODO: result variable and function name variable

    // TODO: modifiers

    TReservedWord.Create(ctx, rwSemiColon, false);

    // TODO: asm
    TBlock.Create(ctx);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
