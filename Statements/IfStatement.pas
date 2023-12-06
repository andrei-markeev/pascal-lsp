unit IfStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, Statement, Expression, ReservedWord;

type
    TIfStatement = class(TToken)
    public
        constructor Create(ctx: ParserContext)
    end;

implementation

constructor TIfStatement.Create(ctx: TParserContext);
var
    hasElse: boolean;
begin
    ctx.Add(Self);
    tokenName := 'If';
    if not PeekReservedWord(ctx, rwIf) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    TReservedWord.Create(ctx, rwIf, true);
    TExpression.Create(ctx);
    TReservedWord.Create(ctx, rwThen, false);
    TStatement.Create(ctx);
    hasElse := PeekReservedWord(ctx, rwElse);
    if hasElse then
    begin
        TReservedWord.Create(ctx, rwElse, true);
        TStatement.Create(ctx);
    end;
end;

end.