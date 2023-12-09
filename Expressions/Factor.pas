unit Factor;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs,
    Token, TypedToken, ReservedWord, Identifier, Number, StringToken;

type
    TFactor = class(TTypedToken)
    public
        unaryOp: TReservedWordKind;
        factorToken: TTypedToken;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind);
    end;

function CreateFactor(ctx: TParserContext; nextTokenKind: TTokenKind): TTypedToken;

implementation

uses Expression;

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
    ident: TIdentifier;
    symbol: TSymbol;
    expr: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'Factor';
    start := ctx.Cursor;
    state := tsCorrect;
    unaryOp := rwUnknown;
    factorToken := nil;
    typeDef.kind := tkUnknown;

    case nextTokenKind.primitiveKind of
        pkNumber:
            begin
                factorToken := TNumber.Create(ctx);
                typeDef := factorToken.typeDef;
            end;
        pkString:
            begin
                factorToken := TStringToken.Create(ctx);
                typeDef := factorToken.typeDef;
            end;
        pkIdentifier:
            begin
                ident := TIdentifier.Create(ctx, true);
                symbol := TSymbol(ident.symbol);
                if symbol <> nil then
                begin
                    case symbol.kind of
                        skUnitName:
                            begin
                                state := tsError;
                                errorMessage := 'Unit name cannot be used in expressions!';
                            end;
                        skTypeName:
                            begin
                                state := tsError;
                                errorMessage := 'Type identifier cannot be used in expressions!';
                            end;
                        skProcedure:
                            begin
                                state := tsError;
                                errorMessage := 'Procedure calls cannot be used in expressions because they don''t have a return type!';
                            end;
                        skFunction:
                            begin
                                // TODO: handle optional parameter list
                                // TODO: check function return type
                                WriteLn('Not implemented!');
                            end;
                        skVariable, skConstant, skTypedConstant:
                            case symbol.typeDef.kind of
                                tkArray:
                                    begin
                                        TReservedWord.Create(ctx, rwOpenSquareBracket, false);
                                        expr := CreateExpression(ctx);
                                        if not TypesAreAssignable(symbol.typeDef.typeOfIndex^, expr.typeDef, errorMessage) then
                                        begin
                                            state := tsError;
                                            errorMessage := 'Invalid array index expression: ' + errorMessage;
                                        end;
                                        typeDef := symbol.typeDef.typeOfValues^;
                                        TReservedWord.Create(ctx, rwCloseSquareBracket, false);
                                    end;
                                tkDynamicArray:
                                    begin
                                        TReservedWord.Create(ctx, rwOpenSquareBracket, false);
                                        expr := CreateExpression(ctx);
                                        if expr.typeDef.kind <> tkInteger then
                                        begin
                                            state := tsError;
                                            errorMessage := 'Dynamic array index access expression should be of an integer type, but it is ' + TypeKindStr[ord(expr.typeDef.kind)];
                                        end;
                                        typeDef := symbol.typeDef.typeOfDynValues^;
                                        TReservedWord.Create(ctx, rwCloseSquareBracket, false);
                                    end;
                                tkRecord, tkObject, tkClass:
                                    begin
                                        // TODO: handle member access
                                        WriteLn('Not implemented!');
                                    end;
                            else
                                factorToken := ident;
                                typeDef := symbol.typeDef;
                            end
                    end;
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
                        // TODO: validate if factor's type can be used with current operator
                        // and change typeDef if needed
                        typeDef := TFactor(factorToken).typeDef;
                    end;
                rwNil:
                    begin
                        TReservedWord.Create(ctx, rwNil, true);
                        typeDef := pointerType;
                    end;
                rwOpenSquareBracket:
                    begin
                        // TODO: constructor of a set
                    end;
                rwOpenParenthesis: 
                    begin
                        TReservedWord.Create(ctx, rwOpenParenthesis, true);
                        unaryOp := rwOpenParenthesis;
                        factorToken := CreateExpression(ctx);
                        TReservedWord.Create(ctx, rwOpenParenthesis, false);
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
