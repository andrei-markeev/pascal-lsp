unit IfStatement;

{$mode objfpc}

interface

uses
    classes, Parser, ParserContext, Statement, Expression, ReservedWord;

type
    TIfStatement = class(TStatement)
    private
        context: TParserContext;
    public
        ifToken: TReservedWord;
        condition: TExpression;
        thenToken: TReservedWord;
        statement: TStatement;
        elseToken: TReservedToken;
        elseStatement: TStatement;
        constructor Create(ctx: ParserContext)
        destructor Destroy; override;
    end;

implementation

constructor TIfStatement.Create(ctx: TParserContext);
var
    hasElse: boolean;
begin
    ifToken := TReservedWord.Create(ctx, 'if');
    condition := TExpression.Create(ctx);
    thenToken := TReservedWord.Create(ctx, 'then');
    statement := TStatement.Create(ctx);
    hasElse := ctx.Peek('else');
    if hasElse then
    begin
        elseToken := TReservedWord.Create(ctx, 'else');
        elseStatement := TStatement.Create(ctx);
    end;
end;

destructor TIfStatement.Destroy;
begin
    ifToken.Free;
    condition.Free;
    thenToken.Free;
    statement.Free;
    if elseToken <> nil then
    begin
        elseToken.Free;
        elseStatement.Free;
    end;
end;

end.