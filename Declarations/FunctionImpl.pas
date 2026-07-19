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
        funcType: TTypeDef;
        selfType: TTypeDef;
        returnType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    ReservedWord, Scopes, Symbols, Parameters, TypeSpec, ParameterDecl, Block, FunctionDecl, TypeDef, RoutineTypeDef;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType, needsToAddChildSymbols: boolean;
    symbolKind: TSymbolKind;
    symbolParent, symbolField, symbol: TSymbol;
    paramDecl: TParameterDecl;
    params: TParameterList;
    i: integer;
    rw: TReservedWord;
    hasMoreParams: boolean;
    s: string;
    overrideResult: TTryAddOverrideResult;
    routineTypeDef: TRoutineTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'Function';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if not (nextReservedWordKind in [rwFunction, rwProcedure, rwConstructor, rwDestructor]) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    needsReturnType := nextReservedWordKind = rwFunction;
    routineTypeDef := TRoutineTypeDef.Create;
    routineTypeDef.rangeToken := Self;
    funcType := routineTypeDef;

    TReservedWord.Create(ctx, nextReservedWordKind, true);
    case nextReservedWordKind of
        rwFunction:
            begin
                symbolKind := skFunction;
                funcType.kind := tkFunction;
            end;
        rwProcedure:
            begin
                tokenName := 'Procedure';
                symbolKind := skProcedure;
                funcType.kind := tkProcedure;
            end;
        rwConstructor:
            begin
                tokenName := 'Constructor';
                symbolKind := skConstructor;
                funcType.kind := tkFunction;
            end;
        rwDestructor:
            begin
                tokenName := 'Destructor';
                symbolKind := skDestructor;
                funcType.kind := tkProcedure;
            end;
    end;

    selfType := nil;
    symbolField := nil;

    nameIdent := TIdentifier.Create(ctx, false);
    typeIdent := nil;
    needsToAddChildSymbols := false;
    symbolParent := FindSymbol(nameIdent);
    if (symbolParent <> nil) and (symbolParent.kind = skTypeName) then
    begin
        typeIdent := nameIdent;
        symbolParent.AddReference(typeIdent);
        if PeekReservedWord(ctx, rwDot) then
        begin
            TReservedWord.Create(ctx, rwDot, true);
            nameIdent := TIdentifier.Create(ctx, false);
            if (symbolParent.typeDef <> nil) and (symbolParent.typeDef.kind in [tkObject, tkClass]) then
            begin
                if (nameIdent.state = tsCorrect) then
                begin
                    SetString(s, nameIdent.start, nameIdent.len);
                    symbolField := FindSymbol(symbolParent, s, ctx.Cursor);
                    if symbolField = nil then
                    begin
                        nameIdent.state := tsError;
                        nameIdent.errorMessage := symbolParent.name + ' doesn''t have a field with name ' + s + '!';
                    end;
                    // TODO: check that implementation is equivalent to declaration i.e. it has
                    // 1. same kind (constructor/destructor/function/procedure)
                    // 2. same parameter names and types
                    // 3. same return type
                    // 4. same modifiers
                end;

                selfType := symbolParent.typeDef;
                needsToAddChildSymbols := true;
            end
            else
            begin
                typeIdent.state := tsError;
                if symbolParent.typeDef <> nil then
                    typeIdent.errorMessage :=  typeIdent.name + ' is of type ' + TypeKindStr[ord(symbolParent.typeDef.kind)] + ' which is not a structured type. Expected class or object!'
                else
                    typeIdent.errorMessage :=  typeIdent.name + ' is not a structured type. Expected class or object!';
            end;
        end
        else
        begin
            typeIdent.state := tsError;
            typeIdent.errorMessage := 'Previously declared type identifier is used as a ' + LowerCase(tokenName) + ' name!';
        end;
    end;

    params := TParameterList.Create;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if nextReservedWordKind = rwOpenParenthesis then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);

        hasMoreParams := false;
        repeat
            paramDecl := TParameterDecl.Create(ctx);
            for i := 0 to length(paramDecl.idents) - 1 do
            begin
                SetString(s, paramDecl.idents[i].start, paramDecl.idents[i].len);
                params.Add(CreateParam(paramDecl.parameterKind, s, paramDecl.typeDef));
            end;

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

    routineTypeDef.parameters := params;

    returnType := unknownType;
    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        CreateTypeSpec(ctx, returnType);
        routineTypeDef.returnType := returnType;
    end
    else
        routineTypeDef.returnType := nil;

    if symbolField <> nil then
    begin
        symbol := symbolField;
        symbol.implementationDecl := nameIdent;
        nameIdent.symbol := symbol;
        nameIdent.tokenName := 'SymbDecl';
    end
    else
    begin
        overrideResult := TryAddOverride(nameIdent, funcType, ctx.Cursor);
        if overrideResult = ovExactDuplicate then
        begin
            nameIdent.state := tsError;
            nameIdent.errorMessage := 'Duplicate subroutine declaration!';
        end
        else
        begin
            symbol := FindSymbol(nameIdent.GetStr(), ctx.Cursor);
            if symbol = nil then
            begin
                symbol := RegisterSymbol(nameIdent, symbolParent, symbolKind, funcType, ctx.Cursor);
                if symbolParent <> nil then
                    symbol.displayName := symbolParent.displayName + '.' + symbol.displayName;
                symbol.rangeToken := Self;
            end;
            symbol.implementationDecl := nameIdent;
        end;
    end;

    // TODO: result variable variable

    // TODO: modifiers

    TReservedWord.Create(ctx, rwSemiColon, false);

    // TODO: asm

    if needsToAddChildSymbols and (symbolParent <> nil) then
        TBlock.Create(ctx, symbolParent.children, selfType, routineTypeDef.returnType)
    else
        TBlock.Create(ctx, [], selfType, routineTypeDef.returnType);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
