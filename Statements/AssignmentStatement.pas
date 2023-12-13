unit AssignmentStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token;

type
    TAssignmentStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, TypedToken, ReservedWord, Expression, VarRef;

constructor TAssignmentStatement.Create(ctx: TParserContext);
var
    expr: TTypedToken;
    ref: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'Assignment';
    start := ctx.Cursor;

    ref := CreateVarRef(ctx);
    
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
