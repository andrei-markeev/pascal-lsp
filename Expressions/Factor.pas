unit Factor;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, TypedToken, ReservedWord, Identifier, Number, StringToken;

type
    TFactor = class(TTypedToken)
    public
        unaryOp: TReservedWordKind;
        factorToken: TTypedToken;
        constructor Create(ctx: TParserContext; nextTokenKind: TTokenKind; createExpression: TCreateTokenFunc);
    end;

function CreateFactor(ctx: TParserContext; nextTokenKind: TTokenKind; createExpression: TCreateTokenFunc): TTypedToken;

implementation

function CreateFactor(ctx: TParserContext; nextTokenKind: TTokenKind; createExpression: TCreateTokenFunc): TTypedToken;
var
    newFactor: TFactor;
begin
    newFactor := TFactor.Create(ctx, nextTokenKind, createExpression);
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

constructor TFactor.Create(ctx: TParserContext; nextTokenKind: TTokenKind; createExpression: TCreateTokenFunc);
var
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
begin
    ctx.Add(Self);
    tokenName := 'Factor';
    start := ctx.Cursor;
    state := tsCorrect;
    unaryOp := rwUnknown;
    factorToken := nil;

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
                ident := TIdentifier.Create(ctx);
                identName := ident.GetName;
                symbol := FindSymbol(identName);
                if symbol <> nil then
                    symbol.AddReference(ident);
                if symbol = nil then
                begin
                    state := tsError;
                    errorMessage := 'Identifier has not been declared!';
                end
                else if symbol.kind = skUnitName then
                begin
                    state := tsError;
                    errorMessage := 'Unit name cannot be used in expressions!';
                end
                else if symbol.kind = skTypeName then
                begin
                    state := tsError;
                    errorMessage := 'Type identifier cannot be used in expressions!';
                end
                else if symbol.kind = skProcedure then
                begin
                    state := tsError;
                    errorMessage := 'Procedure calls cannot be used in expressions because they don''t have a return type!';
                end
                else if symbol.kind = skFunction then
                begin
                    // TODO: handle optional parameter list
                    // TODO: check function return type
                    WriteLn('Not implemented!');
                end
                else if symbol.typeDef.kind in [tkArray, tkDynamicArray] then
                begin
                    // TODO: handle index expression
                    WriteLn('Not implemented!');
                end
                else if symbol.typeDef.kind in [tkRecord, tkObject, tkClass] then
                begin
                    // TODO: handle member access
                    WriteLn('Not implemented!');
                end
                else
                begin
                    typeDef := symbol.typeDef;
                    factorToken := ident;
                end;
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwNot, rwPlus, rwMinus, rwAt:
                    begin
                        TReservedWord.Create(ctx, nextTokenKind.reservedWordKind, true);
                        unaryOp := nextTokenKind.reservedWordKind;
                        nextTokenKind := DetermineNextTokenKind(ctx);
                        factorToken := TFactor.Create(ctx, nextTokenKind, createExpression);
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
                        factorToken := createExpression(ctx);
                        TReservedWord.Create(ctx, rwOpenParenthesis, false);
                    end;
            else
                state := tsMissing;
                len := 0;
                exit;
            end
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
