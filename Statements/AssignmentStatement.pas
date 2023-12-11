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
    TypeDefs, TypedToken, ReservedWord, Identifier, Expression, VarRef;

constructor TAssignmentStatement.Create(ctx: TParserContext);
var
    expr: TTypedToken;
    ref: TTypedToken;
    typesAreCompatible: boolean;
begin
    ctx.Add(Self);
    tokenName := 'Assignment';
    start := ctx.Cursor;

    ref := CreateVarRef(ctx);
    
    TReservedWord.Create(ctx, rwAssign, false);

    expr := CreateExpression(ctx);

    typesAreCompatible := ref.typeDef.kind = expr.typeDef.kind;
    if (ref.typeDef.kind = tkEnum) and (expr.typeDef.kind in [tkEnum, tkEnumMember]) then
        typesAreCompatible := ref.typeDef.enumSpec = expr.typeDef.enumSpec;
    if (ref.typeDef.kind = tkString) and (expr.typeDef.kind in [tkChar, tkCharRange]) then
        typesAreCompatible := true;
    // TODO: more type compatibility checks

    if not typesAreCompatible then
    begin
        state := tsError;
        if (ref.typeDef.kind = tkEnum) and (ref.typeDef.kind = expr.typeDef.kind) then
            errorMessage := 'Assigning values between different enums is not supported!'
        else
            errorMessage := 'Cannot assign value of type ' + TypeKindStr[ord(expr.typeDef.kind)] + ' to a variable of type ' + TypeKindStr[ord(ref.typeDef.kind)] + '!';
    end
    else
        state := tsCorrect;

    ctx.MarkEndOfToken(Self);
end;

end.
