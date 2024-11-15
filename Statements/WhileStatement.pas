unit WhileStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TWhileStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, TypedToken, ReservedWord, Expression, Statement;

constructor TWhileStatement.Create(ctx: TParserContext);
var
    expr: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'While';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwWhile) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwWhile, true);
    expr := CreateExpression(ctx);
    if expr.typeDef.kind <> tkBoolean then
    begin
        state := tsError;
        errorMessage := 'While condition should be boolean, but it is ' + TypeKindStr[ord(expr.typeDef.kind)] + '.';
    end;

    TReservedWord.Create(ctx, rwDo, false);
    CreateStatement(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
