unit AssignmentStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, TypeDefs, Token, ReservedWord, Identifier, Expression;

type
    TAssignmentStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext; varSymbol: TSymbol);
    end;

implementation

constructor TAssignmentStatement.Create(ctx: TParserContext; varSymbol: TSymbol);
var
    ident: TIdentifier;
begin
    ctx.Add(Self);
    tokenName := 'Assignment';
    start := ctx.Cursor;

    ident := TIdentifier.Create(ctx);
    varSymbol.AddReference(ident);
    TReservedWord.Create(ctx, rwAssign, false);
    TExpression.Create(ctx);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
