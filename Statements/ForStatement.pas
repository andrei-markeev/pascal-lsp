unit ForStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Symbols, Token, TypedToken, ReservedWord, Identifier;

type
    TForStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Expression, Statement;

constructor TForStatement.Create(ctx: TParserContext);
var
    ident: TIdentifier;
    symbol: TSymbol;
    nextReservedWord: TReservedWordKind;
    expr: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'For';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwFor) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwFor, true);
    ident := TIdentifier.Create(ctx, true);
    symbol := TSymbol(ident.symbol);
    if (symbol <> nil) and (symbol.typeDef.kind <> tkInteger) then
    begin
        state := tsError;
        errorMessage := 'Expected loop variable to be of type integer, but ' + ident.name + ' is ' + TypeKindStr[ord(symbol.typeDef.kind)] + '.';
    end;

    TReservedWord.Create(ctx, rwAssign, false);

    expr := CreateExpression(ctx);
    if expr.typeDef.kind <> tkInteger then
    begin
        state := tsError;
        errorMessage := 'Expected initial value of the loop to be of type integer, but it is ' + TypeKindStr[ord(symbol.typeDef.kind)] + '.';
    end;

    nextReservedWord := DetermineReservedWord(ctx);
    if nextReservedWord in [rwTo, rwDownto] then
        TReservedWord.Create(ctx, nextReservedWord, true)
    else
        TReservedWord.Create(ctx, rwTo, false);

    expr := CreateExpression(ctx);

    TReservedWord.Create(ctx, rwDo, false);
    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
