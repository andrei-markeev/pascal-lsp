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

uses TypeDefs, Scopes, Symbols, ReservedWord, Identifier, Statement;

constructor TWithStatement.Create(ctx: TParserContext);
var
    ident: TIdentifier;
    symbol: TSymbol;
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
    if (symbol <> nil) and not (symbol.typeDef.kind in [tkRecord, tkObject, tkClass]) then
    begin
        state := tsError;
        errorMessage := 'Operator ''with'' cannot be applied to a variable of type ' + TypeKindStr[ord(symbol.typeDef.kind)] + '!';
    end;
    TReservedWord.Create(ctx, rwDo, false);

    RegisterScope(Self);
    for i := 0 to length(symbol.children) - 1 do
        RegisterSymbol(symbol.children[i].declaration, nil, symbol.children[i].kind, symbol.children[i].typeDef, start);

    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
