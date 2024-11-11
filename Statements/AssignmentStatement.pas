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
    TypeDefs, ReservedWord, Expression, VarRef, Identifier;

constructor TAssignmentStatement.Create(ctx: TParserContext; ref: TTypedToken);
var
    expr: TTypedToken;
    symbol: TSymbol;
    typeError: string;
begin
    ctx.InsertBefore(ref, Self);
    tokenName := 'Assignment';
    start := ref.start;
    state := tsCorrect;

    if ref is TVarRef then
        symbol := TSymbol(TVarRef(ref).firstIdent.symbol)
    else
        symbol := TSymbol(TIdentifier(ref).symbol);

    if (symbol <> nil) and (symbol.kind in [skConstant, skTypedConstant]) then
    begin
        state := tsError;
        errorMessage := 'Cannot modify a constant!';
    end;

    TReservedWord.Create(ctx, rwAssign, false);

    expr := CreateExpression(ctx);

    if not TypesAreAssignable(ref.typeDef, expr.typeDef, typeError) then
    begin
        state := tsError;
        errorMessage := 'Invalid assignment: ' + typeError;
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
