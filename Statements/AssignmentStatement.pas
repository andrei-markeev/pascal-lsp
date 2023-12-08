unit AssignmentStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, TypeDefs, Token, TypedToken, ReservedWord, Identifier, Expression;

type
    TAssignmentStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext; varSymbol: TSymbol);
    end;

implementation

constructor TAssignmentStatement.Create(ctx: TParserContext; varSymbol: TSymbol);
var
    ident: TIdentifier;
    expr: TTypedToken;
    typesAreCompatible: boolean;
begin
    ctx.Add(Self);
    tokenName := 'Assignment';
    start := ctx.Cursor;

    // TODO: support member access
    ident := TIdentifier.Create(ctx);
    varSymbol.AddReference(ident);
    TReservedWord.Create(ctx, rwAssign, false);
    expr := CreateExpression(ctx);

    typesAreCompatible := varSymbol.typeDef.kind = expr.typeDef.kind;
    if (varSymbol.typeDef.kind = tkEnum) and (expr.typeDef.kind in [tkEnum, tkEnumMember]) then
        typesAreCompatible := varSymbol.typeDef.enumSpec = expr.typeDef.enumSpec;
    if (varSymbol.typeDef.kind = tkString) and (expr.typeDef.kind in [tkChar, tkCharRange]) then
        typesAreCompatible := true;
    // TODO: more type compatibility checks

    if not typesAreCompatible then
    begin
        state := tsError;
        if (varSymbol.typeDef.kind = tkEnum) and (varSymbol.typeDef.kind = expr.typeDef.kind) then
            errorMessage := 'Assigning values between different enums is not supported!'
        else
            errorMessage := 'Cannot assign value of type ' + TypeKindStr[ord(expr.typeDef.kind)] + ' to a variable of type ' + TypeKindStr[ord(varSymbol.typeDef.kind)] + '!';
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
