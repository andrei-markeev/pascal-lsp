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
        constructor Create(ctx: TParserContext);
    end;

function CreateVarRef(ctx: TParserContext): TTypedToken;

implementation

uses
    sysutils, Symbols, TypeDefs, CompilationMode, Token, ReservedWord, Expression;

function CreateVarRef(ctx: TParserContext): TTypedToken;
var
    ref: TVarRef;
begin
    ref := TVarRef.Create(ctx);
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

constructor TVarRef.Create(ctx: TParserContext);
var
    symbol: TSymbol;
    found: pointer;
    ident: TIdentifier;
    varRef: TVarRef;
    expr: TTypedToken;
    reservedWordToken: TReservedWord;
    nextReservedWord: TReservedWordKind;
    error: string;
    text: string;
    canBeTypecast: boolean;
    nextIsComma: boolean;
begin
    ctx.Add(Self);
    tokenName := 'VarRef';
    start := ctx.Cursor;
    state := tsCorrect;
    isSimple := true;

    // TODO: allow using expressions

    firstIdent := TIdentifier.Create(ctx, true);
    symbol := TSymbol(firstIdent.symbol);
    if symbol <> nil then
        typeDef := symbol.typeDef
    else
        typeDef.kind := tkUnknown;

    canBeTypecast := ctx.mode >= cmTurboPascal;

    nextReservedWord := DetermineReservedWord(ctx);

    while nextReservedWord in [rwOpenParenthesis, rwOpenSquareBracket, rwHat, rwDot] do
    begin

        case nextReservedWord of
            rwOpenParenthesis:
                if isSimple and canBeTypecast and (symbol <> nil) and (symbol.kind = skTypeName) then
                begin
                    TReservedWord.Create(ctx, rwOpenParenthesis, true);
                    varRef := TVarRef.Create(ctx);
                    if (varRef.state <> tsError) and (varRef.typeDef.size <> typeDef.size) then
                    begin
                        state := tsError;
                        errorMessage := 'Invalid typecast: type ' + firstIdent.name + '(' + TypeKindStr[ord(typeDef.kind)] + ') has size ' + IntToStr(typeDef.size) + ' but the typecasted variable reference has size ' + IntToStr(varRef.typeDef.size);
                    end;

                    TReservedWord.Create(ctx, rwCloseParenthesis, true);

                    isSimple := false;
                end
                else
                    break;
            rwOpenSquareBracket:
                begin
                    reservedWordToken := TReservedWord.Create(ctx, rwOpenSquareBracket, true);

                    if not (typeDef.kind in [tkArray, tkDynamicArray]) then
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
                        if (typeDef.kind = tkArray) and not TypesAreAssignable(typeDef.typeOfIndex^, expr.typeDef, error) then
                        begin
                            expr.state := tsError;
                            expr.errorMessage := 'Index expression is not compatible with the array type: ' + error;
                        end;

                        if typeDef.kind = tkArray then
                            typeDef := typeDef.typeOfValues^
                        else if typeDef.kind = tkDynamicArray then
                            typeDef := typeDef.typeOfDynValues^;

                        nextIsComma := PeekReservedWord(ctx, rwComma);
                        if nextIsComma then
                            TReservedWord.Create(ctx, rwComma, true);
                    until not nextIsComma;

                    TReservedWord.Create(ctx, rwCloseSquareBracket, false);
                    isSimple := false;
                end;
            rwHat:
                begin
                    if typeDef.kind <> tkPointer then
                    begin
                        state := tsError;
                        if isSimple then
                            errorMessage := 'Expected a pointer, but found a ' + SymbolKindStr[ord(symbol.kind)]
                        else
                        begin
                            SetString(text, start, ctx.Cursor - start);
                            errorMessage := 'Cannot dereference ' + TypeKindStr[ord(typeDef.kind)] + ' ' + text + ' because it is not a pointer!';
                        end;
                    end
                    else if (typeDef.kind = tkPointer) and not typeDef.isTyped then
                    begin
                        state := tsError;
                        errorMessage := 'Cannot dereference an untyped pointer! You might want to typecast it to a typed pointer first.';
                    end
                    else
                        typeDef := typeDef.pointerToType^;

                    TReservedWord.Create(ctx, rwHat, true);

                    isSimple := false;
                end;
            rwDot:
                begin
                    reservedWordToken := TReservedWord.Create(ctx, rwDot, true);

                    if not (typeDef.kind in [tkRecord, tkClass, tkObject]) then
                    begin
                        SetString(text, start, ctx.Cursor - start - 1);
                        reservedWordToken.state := tsError;
                        reservedWordToken.errorMessage := 'Cannot apply ''.'' on ' + text + ' because it is not of a structured type (record, class or object)!';
                    end;

                    ident := TIdentifier.Create(ctx, false);
                    if typeDef.kind in [tkRecord, tkClass, tkObject] then
                    begin
                        text := ident.GetStr();
                        found := typeDef.fields.Find(text);
                        if found = nil then
                        begin
                            ident.state := tsError;
                            ident.errorMessage := 'Field or method with the name ''' + text + ''' was not found!';
                            typeDef.size := 1;
                            typeDef.kind := tkUnknown;
                        end
                        else
                            typeDef := PTypeDef(found)^;
                    end;
                    isSimple := false;
                end;
        end;

        nextReservedWord := DetermineReservedWord(ctx);

    end;

    ctx.MarkEndOfToken(Self);
end;

end.
