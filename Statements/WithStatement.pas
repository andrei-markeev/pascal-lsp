unit WithStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Symbols, CommonFuncs, Token, ReservedWord, Identifier;

type
    TWithStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TWithStatement.Create(ctx: TParserContext);
var
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
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
    ident := TIdentifier.Create(ctx);
    identName := ident.GetName;
    symbol := FindSymbol(identName);
    if symbol = nil then
    begin
        state := tsError;
        errorMessage := 'Identifier has not been declared!';
    end;
    symbol.AddReference(ident);
    if not (symbol.typeDef.kind in [tkRecord, tkObject, tkClass]) then
    begin
        state := tsError;
        errorMessage := 'Operator "with" cannot be applied to a variable of type ' + TypeKindStr[ord(symbol.typeDef.kind)] + '!';
    end;
    TReservedWord.Create(ctx, rwDo, false);

    // TODO: add members of the `symbol` to the scope of the statement
    CommonFunctions.createStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.