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
    Symbols, TypeDefs, Token, Identifier, Number, StringToken,
    Expression, VarRef, SetConstructor;

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
    typeDef := Default(TTypeDef);

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
                identName := PeekIdentifier(ctx);
                symbol := FindSymbol(identName);
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
                                errorMessage := 'Invalid call to ' + identName + ': procedure calls cannot be used in expressions because they don''t have a return value!';
                            end;
                        skFunction:
                            begin
                                // TODO: handle optional parameter list
                                // TODO: check function return type
                                WriteLn('Not implemented!');
                            end;
                        skVariable, skConstant, skTypedConstant:
                            begin
                                factorToken := CreateVarRef(ctx);
                                typeDef := factorToken.typeDef;
                            end;
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
                        typeDef := pointer64Type;
                    end;
                rwOpenSquareBracket:
                    begin
                        factorToken := TSetConstructor.Create(ctx);
                        typeDef := TFactor(factorToken).typeDef;
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
