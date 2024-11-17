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
        selfType: PTypeDef;
        returnType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    ReservedWord, Scopes, Symbols, Parameters, TypeSpec, ParameterDecl, Block;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextReservedWordKind: TReservedWordKind;
    needsReturnType, needsToAddChildSymbols: boolean;
    symbolKind: TSymbolKind;
    symbolParent, symbolField: TSymbol;
    paramDecl: TParameterDecl;
    params: TParameterList;
    i: integer;
    rw: TReservedWord;
    hasMoreParams: boolean;
    s: string;
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
            if symbolParent.typeDef^.kind in [tkObject, tkClass] then
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
                typeIdent.errorMessage :=  typeIdent.name + ' is of type ' + TypeKindStr[ord(symbolParent.typeDef^.kind)] + ' which is not a structured type. Expected class or object!';
            end;
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
                params.Add(CreateParam(paramDecl.parameterKind, s, @paramDecl.typeDef));
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

    funcType.parameters := params;

    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        CreateTypeSpec(ctx, returnType);
        funcType.returnType := @returnType;
    end
    else
        funcType.returnType := nil;

    RegisterSymbol(nameIdent, symbolParent, symbolKind, @funcType, ctx.Cursor);

    // TODO: result variable variable

    // TODO: modifiers
{
        case s of
            // TODO: external
            'far': funcModifiers.far := true;
            'forward': funcModifiers.forward := true; // TODO: forward is replacing body in FunctionImpl
            'interrupt': funcModifiers.interrupt := true; // TODO: interrupt can only be applied to procedures
            'near': funcModifiers.near := true;
            // TODO: public
        end;
}

    TReservedWord.Create(ctx, rwSemiColon, false);

    // TODO: asm

    if needsToAddChildSymbols then
        TBlock.Create(ctx, symbolParent.children, selfType, funcType.returnType)
    else
        TBlock.Create(ctx, [], selfType, funcType.returnType);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
