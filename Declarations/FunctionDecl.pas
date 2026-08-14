unit FunctionDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Modifiers, Symbols, Parameters, Token, ReservedWord, TypedToken, TypeDef, TypeDefs, Identifier;

type
    TFunctionDecl = class(TToken)
    public
        nameIdent: TIdentifier;
        funcType: TTypeDef;
        returnType: TTypeDef;
        funcModifiers: TFunctionModifiers;
        methodModifiers: TMethodModifiers;
        isOberonMethod: boolean;
        constructor Create(ctx: TParserContext; functionRWKind: TReservedWordKind; parentSymbols: array of TSymbol);
    end;

implementation

uses
    CompilationMode, Scopes, TypeSpec, ParameterDecl, RoutineTypeDef, StructuredTypeDef, PointerTypeDef, TranspileRegister;

constructor TFunctionDecl.Create(ctx: TParserContext; functionRWKind: TReservedWordKind; parentSymbols: array of TSymbol);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType: boolean;
    symbolKind: TSymbolKind;
    paramDecl: TParameterDecl;
    params: TParameterList;
    i, p: integer;
    s, selfTypeName: string;
    rw, openParenTok, closeParenTok: TReservedWord;
    ident: TIdentifier;
    hasMoreParams: boolean;
    isMethodModifier, isFunctionModifier: boolean;
    overrideResult: TTryAddOverrideResult;
    symbol, firstParent, oberonReceiver: TSymbol;
    routineTypeDef: TRoutineTypeDef;
    receiverTypeDef: TTypeDef;
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
    routineTypeDef := TRoutineTypeDef.Create(ctx);
    routineTypeDef.rangeToken := Self;
    funcType := routineTypeDef;

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

    params := TParameterList.Create;

    isOberonMethod := false;
    oberonReceiver := nil;
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
                oberonReceiver := TSymbol(receiverTypeDef.typeSymbol);
                selfTypeName := oberonReceiver.name;
            end;
        end;

        RegisterOberonReceiver(Self, openParenTok.start, (closeParenTok.start + closeParenTok.len) - openParenTok.start, selfTypeName);
    end;

    nameIdent := TIdentifier.Create(ctx, false);

    SetString(s, nameIdent.start, nameIdent.len);
    if (nameIdent.state <> tsError) and (symbolKind = skDestructor) and (LowerCase(s) <> 'destroy') then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Destructor must be called ''Destroy''!';
    end;

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

    firstParent := oberonReceiver;
    if (firstParent = nil) and (length(parentSymbols) > 0) then
        firstParent := parentSymbols[0];

    returnType := unknownType;
    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        CreateTypeSpec(ctx, returnType);
    end
    else if (symbolKind = skConstructor) and (firstParent <> nil) then
        returnType := firstParent.typeDef;

    routineTypeDef.returnType := returnType;

    overrideResult := TryAddOverride(nameIdent, funcType, ctx.Cursor, firstParent);
    if overrideResult = ovExactDuplicate then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Duplicate subroutine declaration!';
    end
    else if overrideResult <> ovAdded then
    begin
        if oberonReceiver <> nil then
        begin
            symbol := RegisterSymbol(nameIdent, oberonReceiver, symbolKind, funcType, ctx.Cursor);
            symbol.rangeToken := Self;
            if (oberonReceiver.typeDef <> nil) and (oberonReceiver.typeDef is TStructuredTypeDef) then
            begin
                if TStructuredTypeDef(oberonReceiver.typeDef).FindMember(nameIdent.GetStr()) = nil then
                    TStructuredTypeDef(oberonReceiver.typeDef).AddMember(nameIdent.GetStr(), funcType);
            end;
        end
        else if length(parentSymbols) = 0 then
        begin
            symbol := RegisterSymbol(nameIdent, nil, symbolKind, funcType, ctx.Cursor);
            symbol.rangeToken := Self;
        end
        else
            for p := 0 to length(parentSymbols) - 1 do
            begin
                symbol := RegisterSymbol(nameIdent, parentSymbols[p], symbolKind, funcType, ctx.Cursor);
                symbol.rangeToken := Self;
            end;
    end;

    TReservedWord.Create(ctx, rwSemiColon, false);

    repeat
        ctx.SkipTrivia;
        s := LowerCase(PeekIdentifier(ctx));
        isMethodModifier := true;
        case s of
            'abstract': methodModifiers.abstract := true;
            'dynamic': methodModifiers.dynamic := true; // TODO: not valid for objects
            'override': methodModifiers.override := true; // TODO: not valid for objects
            'reintroduce': methodModifiers.reintroduce := true; // TODO: not valid for objects
            'virtual': methodModifiers.virtual := true; // TODO: in Turbo Pascal mode, can be followed by a number constant
            'static': methodModifiers.static := true;
            // TODO: message
        else
            isMethodModifier := false;
        end;

        isFunctionModifier := true;
        case s of
            // TODO: alias
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

        if (length(parentSymbols) = 0) and (oberonReceiver = nil) and isMethodModifier then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Method modifier ''' + s + ''' can only be used with class and object methods!';
        end;

        if (s = 'static') and not (mfStaticMethods in Features[ctx.mode]) then
        begin
            ident.state := tsError;
            ident.errorMessage := '''static'' modifier is not supported in this compilation mode!';
        end;

        if ((length(parentSymbols) > 0) or (oberonReceiver <> nil)) and (s = 'export') then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Methods cannot be exported!';
        end;

    until ctx.IsEOF;

    routineTypeDef.isStatic := methodModifiers.static;

    if (nameIdent.state = tsCorrect) and (symbolKind = skDestructor) and not methodModifiers.override then
    begin
        nameIdent.state := tsError;
        nameIdent.errorMessage := 'Destructor must have ''override'' modifier!';
    end;

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
