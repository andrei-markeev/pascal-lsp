unit WithStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TWithStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses TypeDefs, TypeDef, Scopes, Symbols, ReservedWord, Identifier, Statement;

constructor TWithStatement.Create(ctx: TParserContext);
var
    ident: TIdentifier;
    symbol, targetSymbol: TSymbol;
    returnTypeDef, targetTypeDef: TTypeDef;
    i: integer;
begin
    ctx.Add(Self);
    tokenName := 'With';
    if not PeekReservedWord(ctx, rwWith) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwWith, true);
    ident := TIdentifier.Create(ctx, true);

    symbol := TSymbol(ident.symbol);
    targetTypeDef := nil;
    targetSymbol := nil;

    if symbol <> nil then
    begin
        returnTypeDef := symbol.GetCurrentReturnType(ctx);
        if returnTypeDef <> nil then
        begin
            targetTypeDef := returnTypeDef;
            targetSymbol := TSymbol(targetTypeDef.typeSymbol);
        end
        else
        begin
            targetTypeDef := symbol.typeDef;
            targetSymbol := symbol;
            if (length(targetSymbol.children) = 0) and (targetTypeDef <> nil) then
                targetSymbol := TSymbol(targetTypeDef.typeSymbol);
        end;
    end;

    if (targetTypeDef <> nil) and not (targetTypeDef.kind in [tkRecord, tkObject, tkClass]) then
    begin
        state := tsError;
        errorMessage := 'Operator ''with'' cannot be applied to a variable of type ' + TypeKindStr[ord(targetTypeDef.kind)] + '!';
    end;
    TReservedWord.Create(ctx, rwDo, false);

    RegisterScope(Self);
    if targetSymbol <> nil then
        for i := 0 to length(targetSymbol.children) - 1 do
            if (targetSymbol.children[i].typeDef <> nil) and (targetSymbol.children[i].typeDef.visibility = vPublic) then // TODO: handle `with Self`
                RegisterSymbol(targetSymbol.children[i].declaration, nil, targetSymbol.children[i].kind, targetSymbol.children[i].typeDef, start);

    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
