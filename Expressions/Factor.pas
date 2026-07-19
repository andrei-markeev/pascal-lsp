unit Factor;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypedToken, ReservedWord;

type
    TFactor = class(TTypedToken)
    public
        unaryOp: TReservedWordKind;
        factorToken: TTypedToken;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind);
    end;

function CreateFactor(ctx: TParserContext; nextTokenKind: TTokenKind): TTypedToken;

implementation

uses
    CompilationMode, Symbols, TypeDefs, TypeDef, Token, Identifier, Number, StringToken,
    Expression, VarRef, Call, SetConstructor, RoutineTypeDef;

function CreateFactor(ctx: TParserContext; nextTokenKind: TTokenKind): TTypedToken;
var
    newFactor: TFactor;
begin
    newFactor := TFactor.Create(ctx, nextTokenKind);
    if (newFactor.unaryOp = rwUnknown) and (newFactor.factorToken <> nil) then
    begin
        CreateFactor := newFactor.factorToken;
        newFactor.state := tsInvisible;
        if newFactor.endMarker <> nil then
            newFactor.endMarker.state := tsInvisible;
    end
    else
        CreateFactor := newFactor;
end;

constructor TFactor.Create(ctx: TParserContext; nextTokenKind: TTokenKind);
var
    identName: shortstring;
    symbol: TSymbol;
begin
    ctx.Add(Self);
    tokenName := 'Factor';
    start := ctx.Cursor;
    state := tsCorrect;
    unaryOp := rwUnknown;
    factorToken := nil;
    typeDef := unknownType;

    case nextTokenKind.primitiveKind of
        pkNumber:
            begin
                factorToken := TNumber.Create(ctx);
                if factorToken <> nil then
                    typeDef := factorToken.typeDef;
            end;
        pkString:
            begin
                factorToken := TStringToken.Create(ctx);
                if factorToken <> nil then
                    typeDef := factorToken.typeDef;
            end;
        pkIdentifier:
            begin
                identName := PeekIdentifier(ctx);
                symbol := FindSymbol(identName, ctx.Cursor);

                factorToken := CreateVarRef(ctx);
                if (factorToken <> nil) and (factorToken.typeDef <> nil) then
                    typeDef := factorToken.typeDef;

                if (symbol <> nil) and (symbol.kind = skUnitName) then
                begin
                    state := tsError;
                    errorMessage := 'Unit name cannot be used in expressions!';
                end
                else if (symbol <> nil) and (symbol.kind = skProcedure) and (ctx.mode <> cmMacPascal) then
                begin
                    state := tsError;
                    errorMessage := 'Invalid call to ' + identName + ': procedure calls cannot be used in expressions because they don''t have a return value!';
                    if PeekReservedWord(ctx, rwOpenParenthesis) then
                        factorToken := TCall.Create(ctx, factorToken);
                end
                else if ((typeDef <> nil) and (typeDef.kind = tkFunction) and (typeDef is TRoutineTypeDef))
                     or PeekReservedWord(ctx, rwOpenParenthesis) then
                begin
                    if (typeDef <> nil) and (typeDef.kind = tkFunction) and (typeDef is TRoutineTypeDef) then
                    begin
                        if TRoutineTypeDef(typeDef).returnType <> nil then
                            typeDef := TRoutineTypeDef(typeDef).returnType
                        else
                            typeDef := unknownType;
                    end
                    else
                        typeDef := unknownType;
                    factorToken := TCall.Create(ctx, factorToken);
                end;
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwNot, rwPlus, rwMinus, rwAt:
                    begin
                        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
                        unaryOp := nextTokenKind.reservedWordKind;
                        nextTokenKind := DetermineNextTokenKind(ctx);
                        factorToken := TFactor.Create(ctx, nextTokenKind);
                        if (factorToken <> nil) and (factorToken.typeDef <> nil) then
                            typeDef := TFactor(factorToken).typeDef;
                    end;
                rwNil:
                    begin
                        TReservedWord.Create(ctx, rwNil, true);
                        typeDef := pointer64Type;
                    end;
                rwOpenSquareBracket:
                    begin
                        factorToken := TSetConstructor.Create(ctx);
                        if (factorToken <> nil) and (factorToken.typeDef <> nil) then
                            typeDef := factorToken.typeDef;
                    end;
                rwOpenParenthesis: 
                    begin
                        TReservedWord.Create(ctx, rwOpenParenthesis, true);
                        unaryOp := rwOpenParenthesis;
                        factorToken := CreateExpression(ctx);
                        if (factorToken <> nil) and (factorToken.typeDef <> nil) then
                            typeDef := factorToken.typeDef;
                        TReservedWord.Create(ctx, rwCloseParenthesis, false);
                    end;
            else
                start := ctx.GetCursorBeforeTrivia;
                state := tsMissing;
                len := 0;
                exit;
            end
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
