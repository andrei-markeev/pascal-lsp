unit FunctionImpl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypedToken, TypeDef, TypeDefs, Identifier, Modifiers;

type
    TFunctionImpl = class(TToken)
    public
        typeIdent: TIdentifier;
        nameIdent: TIdentifier;
        funcType: TTypeDef;
        selfType: TTypeDef;
        returnType: TTypeDef;
        isOberonMethod: boolean;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    CompilationMode, ReservedWord, Scopes, Symbols, Parameters, TypeSpec, ParameterDecl, Block, FunctionDecl, RoutineTypeDef, RoutineEquivalence, StructuredTypeDef, PointerTypeDef, TranspileRegister;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType, needsToAddChildSymbols, hasOpenParenthesis: boolean;
    symbolKind: TSymbolKind;
    symbolParent, symbolField, symbol: TSymbol;
    paramDecl: TParameterDecl;
    params: TParameterList;
    i: integer;
    rw, openParenTok, closeParenTok: TReservedWord;
    hasMoreParams: boolean;
    s, selfTypeName: string;
    overrideResult: TTryAddOverrideResult;
    routineTypeDef: TRoutineTypeDef;
    funcModifiers: TFunctionModifiers;
    methodModifiers: TMethodModifiers;
    isMethodModifier, isFunctionModifier: boolean;
    ident: TIdentifier;
    isMethod: boolean;
    receiverTypeDef: TTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'Function';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    if (ctx.tokensLen >= 2) and (ctx.Tokens[ctx.tokensLen - 2] is TReservedWord) then
    begin
        rw := TReservedWord(ctx.Tokens[ctx.tokensLen - 2]);
        if (rw.kind = rwClass) and (rw.state = tsSkipped) then
        begin
            if mfClassMethods in Features[ctx.mode] then
                rw.state := tsCorrect
            else
            begin
                rw.state := tsError;
                rw.errorMessage := 'Class methods are not supported in this compilation mode!';
            end;
            start := rw.start;
        end;
    end;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if not (nextReservedWordKind in [rwFunction, rwProcedure, rwConstructor, rwDestructor]) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    needsReturnType := nextReservedWordKind = rwFunction;
    routineTypeDef := TRoutineTypeDef.Create(ctx);
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

    params := TParameterList.Create;

    selfType := nil;
    symbolParent := nil;
    symbolField := nil;
    isOberonMethod := false;
    if (mfOberonMethodSyntax in Features[ctx.mode]) and PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        isOberonMethod := true;
        openParenTok := TReservedWord.Create(ctx, rwOpenParenthesis, true);
        paramDecl := TParameterDecl.Create(ctx);
        closeParenTok := TReservedWord.Create(ctx, rwCloseParenthesis, false);

        selfTypeName := '';
        if paramDecl.typeDef <> nil then
        begin
            receiverTypeDef := paramDecl.typeDef;
            if (receiverTypeDef is TPointerTypeDef) and (TPointerTypeDef(receiverTypeDef).pointerToType <> nil) then
                receiverTypeDef := TPointerTypeDef(receiverTypeDef).pointerToType;
            if receiverTypeDef.typeSymbol <> nil then
            begin
                symbolParent := TSymbol(receiverTypeDef.typeSymbol);
                selfTypeName := symbolParent.name;
            end;
        end;

        RegisterOberonReceiver(Self, openParenTok.start, (closeParenTok.start + closeParenTok.len) - openParenTok.start, selfTypeName);

        nameIdent := TIdentifier.Create(ctx, false);
        if (symbolParent <> nil) and (nameIdent.state = tsCorrect) then
        begin
            SetString(s, nameIdent.start, nameIdent.len);
            symbolField := FindSymbol(symbolParent, s, ctx.Cursor);
        end;
    end
    else
    begin
        nameIdent := TIdentifier.Create(ctx, false);
        typeIdent := nil;
        needsToAddChildSymbols := false;
        symbolParent := FindSymbol(nameIdent);
    end;

    if not isOberonMethod and (symbolParent <> nil) and (symbolParent.kind = skTypeName) then
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

    hasOpenParenthesis := false;
    nextReservedWordKind := DetermineReservedWord(ctx);
    if nextReservedWordKind = rwOpenParenthesis then
    begin
        hasOpenParenthesis := true;
        TReservedWord.Create(ctx, rwOpenParenthesis, true);

        hasMoreParams := false;
        repeat
            paramDecl := TParameterDecl.Create(ctx);
            for i := 0 to length(paramDecl.idents) - 1 do
            begin
                SetString(s, paramDecl.idents[i].start, paramDecl.idents[i].len);
                params.Add(CreateParam(paramDecl.parameterKind, s, paramDecl.typeDef, paramDecl.hasDefaultValue));
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

    TReservedWord.Create(ctx, rwSemiColon, false);

    FillChar(funcModifiers, SizeOf(funcModifiers), 0);
    FillChar(methodModifiers, SizeOf(methodModifiers), 0);
    isMethod := (symbolParent <> nil) and (symbolParent.kind = skTypeName);

    repeat
        ctx.SkipTrivia;
        s := LowerCase(PeekIdentifier(ctx));
        isMethodModifier := true;
        case s of
            'abstract': methodModifiers.abstract := true;
            'dynamic': methodModifiers.dynamic := true;
            'override': methodModifiers.override := true;
            'reintroduce': methodModifiers.reintroduce := true;
            'virtual': methodModifiers.virtual := true;
            'static': methodModifiers.static := true;
        else
            isMethodModifier := false;
        end;

        isFunctionModifier := true;
        case s of
            'cdecl': funcModifiers.cdecl := true;
            'cppdecl': funcModifiers.cppdecl := true;
            'export': funcModifiers.export := true;
            'forward': funcModifiers.forward := true;
            'hardfloat': funcModifiers.hardfloat := true;
            'inline': funcModifiers.inline := true;
            'iocheck': funcModifiers.iocheck := true;
            'local': funcModifiers.local := true;
            'MS_ABI_Default': funcModifiers.MS_ABI_Default := true;
            'MS_ABI_CDecl': funcModifiers.MS_ABI_CDecl := true;
            'MWPascal': funcModifiers.MWPascal := true;
            'noreturn': funcModifiers.noreturn := true;
            'nostackframe': funcModifiers.nostackframe := true;
            'overload': funcModifiers.overload := true;
            'pascal': funcModifiers.pascal := true;
            'register': funcModifiers.register := true;
            'safecall': funcModifiers.safecall := true;
            'saveregisters': funcModifiers.saveregisters := true;
            'softload': funcModifiers.softload := true;
            'stdcall': funcModifiers.stdcall := true;
            'SYSV_ABI_Default': funcModifiers.SYSV_ABI_Default := true;
            'SYSV_ABI_CDecl': funcModifiers.SYSV_ABI_CDecl := true;
            'varargs': funcModifiers.varargs := true;
            'vectorcall': funcModifiers.vectorcall := true;
            'winapi': funcModifiers.winapi := true;
        else
            isFunctionModifier := false;
        end;

        if not isMethodModifier and not isFunctionModifier then
            break;

        ident := TIdentifier.Create(ctx, false);
        TReservedWord.Create(ctx, rwSemiColon, false);

        if not isMethod and isMethodModifier then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Method modifier ''' + s + ''' can only be used with class and object methods!';
        end;

        if isMethod and isMethodModifier and (s <> 'static') then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Method modifier ''' + s + ''' cannot be used in implementation!';
        end;

        if (s = 'static') and not (mfStaticMethods in Features[ctx.mode]) then
        begin
            ident.state := tsError;
            ident.errorMessage := '''static'' modifier is not supported in this compilation mode!';
        end;

        if isMethod and (s = 'export') then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Methods cannot be exported!';
        end;

    until ctx.IsEOF;

    if funcModifiers.forward then
    begin
        case symbolKind of
            skFunction: tokenName := 'FunctionDecl';
            skProcedure: tokenName := 'ProcedureDecl';
            skConstructor: tokenName := 'ConstructorDecl';
            skDestructor: tokenName := 'DestructorDecl';
        end;
    end;

    if symbolField <> nil then
    begin
        VerifyImplementationEquivalence(symbolField, symbolKind, routineTypeDef, hasOpenParenthesis, funcModifiers, methodModifiers, nameIdent);
        symbol := symbolField;
        symbol.implementationDecl := nameIdent;
        nameIdent.symbol := symbol;
        nameIdent.tokenName := 'SymbDecl';
        if not funcModifiers.forward then
            symbol.implRangeToken := Self;
    end
    else
    begin
        if (symbolParent <> nil) and (symbolParent.kind = skTypeName) then
            overrideResult := TryAddOverride(nameIdent, funcType, ctx.Cursor, symbolParent)
        else
            overrideResult := TryAddOverride(nameIdent, funcType, ctx.Cursor, nil);
        if overrideResult = ovExactDuplicate then
        begin
            nameIdent.state := tsError;
            nameIdent.errorMessage := 'Duplicate subroutine declaration!';
        end
        else
        begin
            symbol := FindSymbol(nameIdent.GetStr(), ctx.Cursor);
            if (symbol <> nil) and (symbol.rangeToken <> nil) and (symbol.rangeToken <> Self) then
                VerifyImplementationEquivalence(symbol, symbolKind, routineTypeDef, hasOpenParenthesis, funcModifiers, methodModifiers, nameIdent);

            if symbol = nil then
            begin
                symbol := RegisterSymbol(nameIdent, symbolParent, symbolKind, funcType, ctx.Cursor);
                if symbolParent <> nil then
                    symbol.displayName := symbolParent.displayName + '.' + symbol.displayName;
                symbol.rangeToken := Self;
            end;
            symbol.implementationDecl := nameIdent;
            if not funcModifiers.forward then
                symbol.implRangeToken := Self;
        end;
    end;

    if isOberonMethod and (symbolParent <> nil) and (symbolParent.typeDef <> nil) and (symbolParent.typeDef is TStructuredTypeDef) then
    begin
        if TStructuredTypeDef(symbolParent.typeDef).FindMember(nameIdent.GetStr()) = nil then
            TStructuredTypeDef(symbolParent.typeDef).AddMember(nameIdent.GetStr(), funcType);
    end;

    if funcModifiers.forward then
    begin
        state := tsCorrect;
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    // TODO: modifiers

    // TODO: asm

    if methodModifiers.static or ((symbolField <> nil) and (symbolField.typeDef <> nil) and (symbolField.typeDef is TRoutineTypeDef) and TRoutineTypeDef(symbolField.typeDef).isStatic) then
    begin
        routineTypeDef.isStatic := true;
        selfType := nil;
    end;

    if needsToAddChildSymbols and (symbolParent <> nil) then
        TBlock.Create(ctx, symbolParent.children, selfType, routineTypeDef.returnType, Self)
    else
        TBlock.Create(ctx, [], selfType, routineTypeDef.returnType, Self);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
