unit AssignmentStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, TypedToken;

type
    TAssignmentStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext; ref: TTypedToken);
    end;

implementation

uses
    TypeDefs, ReservedWord, Expression, VarRef;

constructor TAssignmentStatement.Create(ctx: TParserContext; ref: TTypedToken);
var
    expr: TTypedToken;
begin
    ctx.InsertBefore(ref, Self);
    tokenName := 'Assignment';
    start := ref.start;
    
    TReservedWord.Create(ctx, rwAssign, false);

    expr := CreateExpression(ctx);

    if not TypesAreAssignable(ref.typeDef, expr.typeDef, errorMessage) then
    begin
        state := tsError;
        errorMessage := 'Invalid assignment: ' + errorMessage;
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
