unit IfStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Token, TypedToken, ReservedWord;

type
    TIfStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Expression, Statement;

constructor TIfStatement.Create(ctx: TParserContext);
var
    hasElse: boolean;
    expr: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'If';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwIf) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwIf, true);
    expr := CreateExpression(ctx);
    if (expr.state <> tsMissing) and (expr.typeDef.kind <> tkBoolean) then
    begin
        state := tsError;
        errorMessage := 'Condition expression returns ' + TypeKindStr[ord(expr.typeDef.kind)] + ' but must return a boolean value!';
    end;
    TReservedWord.Create(ctx, rwThen, false);
    CreateStatement(ctx);
    hasElse := PeekReservedWord(ctx, rwElse);
    if hasElse then
    begin
        TReservedWord.Create(ctx, rwElse, true);
        CreateStatement(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
