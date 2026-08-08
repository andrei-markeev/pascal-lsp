unit VarRef;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken, Identifier, TypeDef, Symbols;

type
    TVarRef = class(TTypedToken)
    public
        symbol: TSymbol;
        canBeTypecast: boolean;
        firstIdent: TIdentifier;
        isSimple: boolean;
        procedure ParseArrayAccess(ctx: TParserContext);
        procedure ParsePointerDereference(ctx: TParserContext);
        constructor Create(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false);
    end;

function CreateVarRef(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false): TTypedToken;

implementation

uses
    sysutils, CompilationMode, Token, Expression, Call,
    PointerTypeDef, ArrayTypeDef, DynamicArrayTypeDef, ClassTypeDef,
    TypeDefs, ReservedWord, InheritedRef, MemberAccess, TypecastRef;

function CreateVarRef(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false): TTypedToken;
var
    ref: TVarRef;
begin
    ref := TVarRef.Create(ctx, baseRef, isMaybeLeftHandSide);
    if ref.isSimple then
    begin
        CreateVarRef := ref.firstIdent;
        ref.state := tsInvisible;
        if ref.endMarker <> nil then
            ref.endMarker.state := tsInvisible;
    end
    else
        CreateVarRef := ref;
end;

procedure TVarRef.ParseArrayAccess(ctx: TParserContext);
var
    reservedWordToken: TReservedWord;
    curType: TTypeDef;
    found: pointer;
    expr: TTypedToken;
    error: string;
    nextIsComma: boolean;
    text: string;
begin
    reservedWordToken := TReservedWord.Create(ctx, rwOpenSquareBracket, true);

    if (typeDef <> nil) and (typeDef.kind = tkClass) then
    begin
        curType := typeDef;
        found := nil;
        while curType <> nil do
        begin
            if (curType.kind = tkClass) and (curType is TClassTypeDef) then
            begin
                found := TClassTypeDef(curType).FindMember('strings');
                if found = nil then
                    found := TClassTypeDef(curType).FindMember('items');
                if found <> nil then
                    break;
                curType := TClassTypeDef(curType).parentClass;
            end
            else
                break;
        end;
        if found <> nil then
            typeDef := TTypeDef(found);
    end;

    if (typeDef = nil) or not ((typeDef.kind in [tkArray, tkDynamicArray, tkString]) or IsPChar(typeDef)) then
    begin
        reservedWordToken.state := tsError;
        if isSimple then
            reservedWordToken.errorMessage := 'Cannot index ' + firstIdent.name + ' because it is not an array!'
        else
        begin
            SetString(text, start, ctx.Cursor - start - 1);
            reservedWordToken.errorMessage := 'Cannot index ' + text + ' because it is not an array!';
        end;
    end;

    repeat
        expr := CreateExpression(ctx);
        if (typeDef <> nil) and (typeDef.kind = tkArray) and (typeDef is TArrayTypeDef) and (TArrayTypeDef(typeDef).typeOfIndex <> nil) and not TypesAreAssignable(ctx, TArrayTypeDef(typeDef).typeOfIndex, expr.typeDef, error) then
        begin
            expr.state := tsError;
            expr.errorMessage := 'Index expression is not compatible with the array type: ' + error;
        end;

        if (typeDef <> nil) and (typeDef.kind = tkArray) and (typeDef is TArrayTypeDef) and (TArrayTypeDef(typeDef).typeOfValues <> nil) then
            typeDef := TArrayTypeDef(typeDef).typeOfValues
        else if (typeDef <> nil) and (typeDef.kind = tkDynamicArray) and (typeDef is TDynamicArrayTypeDef) and (TDynamicArrayTypeDef(typeDef).typeOfDynValues <> nil) then
            typeDef := TDynamicArrayTypeDef(typeDef).typeOfDynValues
        else if (typeDef <> nil) and ((typeDef.kind = tkString) or IsPChar(typeDef)) then
            typeDef := charType
        else
            typeDef := unknownType;

        nextIsComma := PeekReservedWord(ctx, rwComma);
        if nextIsComma then
            TReservedWord.Create(ctx, rwComma, true);
    until not nextIsComma;

    TReservedWord.Create(ctx, rwCloseSquareBracket, false);
    isSimple := false;
end;

procedure TVarRef.ParsePointerDereference(ctx: TParserContext);
var
    text: string;
begin
    if (typeDef = nil) or (typeDef.kind <> tkPointer) then
    begin
        state := tsError;
        if isSimple then
        begin
            if symbol <> nil then
                errorMessage := 'Expected a pointer, but found a ' + SymbolKindStr[ord(symbol.kind)]
            else
                errorMessage := 'Expected a pointer!';
        end
        else
        begin
            SetString(text, start, ctx.Cursor - start);
            if typeDef <> nil then
                errorMessage := 'Cannot dereference ' + TypeKindStr[ord(typeDef.kind)] + ' ' + text + ' because it is not a pointer!'
            else
                errorMessage := 'Cannot dereference ' + text + ' because it is not a pointer!';
        end;
    end
    else if (typeDef is TPointerTypeDef) and not TPointerTypeDef(typeDef).isTyped then
    begin
        state := tsError;
        errorMessage := 'Cannot dereference an untyped pointer! You might want to typecast it to a typed pointer first.';
    end
    else if (typeDef is TPointerTypeDef) and (TPointerTypeDef(typeDef).pointerToType <> nil) then
        typeDef := TPointerTypeDef(typeDef).pointerToType
    else
        typeDef := unknownType;

    TReservedWord.Create(ctx, rwHat, true);
    isSimple := false;
end;

constructor TVarRef.Create(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false);
var
    found: pointer;
    nextReservedWord: TReservedWordKind;
begin
    tokenName := 'VarRef';
    if baseRef <> nil then
    begin
        ctx.InsertBefore(baseRef, Self);
        start := baseRef.start;
    end
    else
    begin
        ctx.Add(Self);
        start := ctx.Cursor;
    end;
    state := tsCorrect;

    if (baseRef = nil) and PeekReservedWord(ctx, rwInherited) then
        ParseInherited(ctx, Self)
    else if baseRef = nil then
    begin
        isSimple := true;
        firstIdent := TIdentifier.Create(ctx, true);
        symbol := TSymbol(firstIdent.symbol);
        if symbol <> nil then
        begin
            if symbol.typeDef <> nil then
            begin
                typeDef := symbol.typeDef;
                firstIdent.typeDef := symbol.typeDef;
            end;
        end
        else
        begin
            found := TypesList.Find(LowerCase(firstIdent.name));
            if found <> nil then
            begin
                typeDef := TTypeDef(found);
                firstIdent.typeDef := TTypeDef(found);
            end
            else
            begin
                typeDef := unknownType;
                firstIdent.typeDef := unknownType;
            end;
        end;
    end
    else
    begin
        isSimple := false;
        firstIdent := nil;
        symbol := nil;
        typeDef := baseRef.typeDef;
        if (baseRef is TCall) and not (ctx.mode in [cmExtendedPascal, cmFreePascal, cmObjectFreePascal, cmDelphi]) then
        begin
            state := tsError;
            errorMessage := 'Cannot access members or index return value of a function call in this compiler mode!';
        end;
    end;

    canBeTypecast := (ctx.mode >= cmTurboPascal) and (baseRef = nil);

    nextReservedWord := DetermineReservedWord(ctx);

    while nextReservedWord in [rwOpenParenthesis, rwOpenSquareBracket, rwHat, rwDot] do
    begin
        if (typeDef <> nil) and (typeDef.kind in [tkProcedure, tkFunction]) then
            break;

        case nextReservedWord of
            rwOpenParenthesis:
                if not ParseTypecast(ctx, Self, isMaybeLeftHandSide) then
                    break;
            rwOpenSquareBracket: ParseArrayAccess(ctx);
            rwHat: ParsePointerDereference(ctx);
            rwDot: ParseDotAccess(ctx, Self);
        end;

        nextReservedWord := DetermineReservedWord(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
