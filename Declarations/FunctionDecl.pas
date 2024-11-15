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
    ident: TIdentifier;
    hasMoreParams: boolean;
    isMethodModifier, isFunctionModifier: boolean;
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
        s := LowerCase(PeekIdentifier(ctx));
        isMethodModifier := true;
        case s of
            'abstract': methodModifiers.abstract := true;
            'dynamic': methodModifiers.dynamic := true; // TODO: not valid for objects
            'override': methodModifiers.override := true; // TODO: not valid for objects
            'reintroduce': methodModifiers.reintroduce := true; // TODO: not valid for objects
            'virtual': methodModifiers.virtual := true; // TODO: in Turbo Pascal mode, can be followed by a number constant
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

        if (length(parentSymbols) = 0) and isMethodModifier then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Method modifier ''' + s + ''' can only be used with class and object methods!';
        end;

        if (length(parentSymbols) > 0) and (s = 'export') then
        begin
            ident.state := tsError;
            ident.errorMessage := 'Methods cannot be exported!';
        end;

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
