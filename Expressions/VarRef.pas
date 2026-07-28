unit VarRef;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken, Identifier;

type
    TVarRef = class(TTypedToken)
    public
        firstIdent: TIdentifier;
        isSimple: boolean;
        constructor Create(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false);
    end;

function CreateVarRef(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false): TTypedToken;

implementation

uses
    sysutils, Symbols, CompilationMode, Token, ReservedWord, Expression, Call, Designator,
    TypeDefs, TypeDef, ClassTypeDef, PointerTypeDef, ArrayTypeDef, DynamicArrayTypeDef, RecordTypeDef, ObjectTypeDef;

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

constructor TVarRef.Create(ctx: TParserContext; baseRef: TTypedToken = nil; isMaybeLeftHandSide: boolean = false);
var
    symbol: TSymbol;
    found: pointer;
    ident: TIdentifier;
    innerToken: TTypedToken;
    expr: TTypedToken;
    reservedWordToken: TReservedWord;
    nextReservedWord: TReservedWordKind;
    error: string;
    text: string;
    canBeTypecast: boolean;
    nextIsComma: boolean;
    curType: TTypeDef;
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

    if baseRef = nil then
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
                if isSimple and canBeTypecast and (
                    ((symbol <> nil) and (symbol.kind = skTypeName)) or
                    ((symbol = nil) and (TypesList.Find(LowerCase(firstIdent.name)) <> nil))
                ) then
                begin
                    TReservedWord.Create(ctx, rwOpenParenthesis, true);
                    if isMaybeLeftHandSide then
                        innerToken := CreateDesignator(ctx, true)
                    else
                        innerToken := CreateExpression(ctx);

                    if (innerToken <> nil) and (innerToken.state <> tsError) and (typeDef <> nil) and (innerToken.typeDef <> nil) and (typeDef.size > 0) and (innerToken.typeDef.size > 0) and (innerToken.typeDef.size <> typeDef.size) then
                    begin
                        state := tsError;
                        errorMessage := 'Invalid typecast: type ' + firstIdent.name + '(' + TypeKindStr[ord(typeDef.kind)] + ') has size ' + IntToStr(typeDef.size) + ' but the typecasted variable reference has size ' + IntToStr(innerToken.typeDef.size);
                    end;

                    TReservedWord.Create(ctx, rwCloseParenthesis, true);

                    isSimple := false;
                end
                else
                    break;
            rwOpenSquareBracket:
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

                    if (typeDef = nil) or not (
                        (typeDef.kind in [tkArray, tkDynamicArray, tkString]) or IsPChar(typeDef)
                    ) then
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
            rwHat:
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
            rwDot:
                begin
                    reservedWordToken := TReservedWord.Create(ctx, rwDot, true);

                    if (typeDef = nil) or not (typeDef.kind in [tkRecord, tkClass, tkObject]) then
                    begin
                        SetString(text, start, ctx.Cursor - start - 1);
                        reservedWordToken.state := tsError;
                        reservedWordToken.errorMessage := 'Cannot apply ''.'' on ' + text + ' because it is not of a structured type (record, class or object)!';
                    end;

                    ident := TIdentifier.Create(ctx, false);
                    if (typeDef <> nil) and (typeDef.kind in [tkRecord, tkClass, tkObject]) then
                    begin
                        text := ident.GetStr();
                        
                        found := nil;
                        curType := typeDef;
                        while curType <> nil do
                        begin
                            case curType.kind of
                                tkRecord:
                                    begin
                                        if curType is TRecordTypeDef then
                                            found := TRecordTypeDef(curType).FindMember(text);
                                        break;
                                    end;
                                tkObject:
                                    begin
                                        if curType is TObjectTypeDef then
                                        begin
                                            found := TObjectTypeDef(curType).FindMember(text);
                                            if found <> nil then
                                                break;
                                            curType := TObjectTypeDef(curType).parentObject;
                                        end
                                        else
                                            break;
                                    end;
                                tkClass:
                                    begin
                                        if curType is TClassTypeDef then
                                        begin
                                            found := TClassTypeDef(curType).FindMember(text);
                                            if found <> nil then
                                                break;
                                            curType := TClassTypeDef(curType).parentClass;
                                        end
                                        else
                                            break;
                                    end;
                            else
                                curType := nil;
                            end;
                        end;

                        if found = nil then
                        begin
                            ident.state := tsError;
                            ident.errorMessage := 'Field or method with the name ''' + text + ''' was not found!';
                            typeDef := unknownType;
                        end
                        else
                        begin
                            typeDef := TTypeDef(found);

                            // TODO: handle valid cases such as Self.privateField
                            if (typeDef <> nil) and (typeDef.visibility in [vPrivate, vProtected]) then
                            begin
                                ident.state := tsError;
                                ident.errorMessage := text + ' is not public, it cannot be used here!';
                            end;
                        end;
                    end;
                    isSimple := false;
                end;
        end;

        nextReservedWord := DetermineReservedWord(ctx);

    end;

    ctx.MarkEndOfToken(Self);
end;

end.
