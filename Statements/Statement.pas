unit Statement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token;

function CreateStatement(ctx: TParserContext): TToken;
function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;

implementation

uses
    TypeDefs, TypeDef, TypedToken, ReservedWord, VarRef, Call, Designator,
    AssignmentStatement, CaseStatement, IfStatement, WithStatement, ForStatement,
    WhileStatement, RepeatStatement, TryStatement, CompoundStatement;

function CreateStatement(ctx: TParserContext): TToken;
begin
    CreateStatement := CreateStatement(ctx, DetermineNextTokenKind(ctx));
end;

function CreateDesignatorStatement(ctx: TParserContext): TToken;
var
    varRef: TTypedToken;
begin
    // This is either an assignment or a procedure call
    varRef := CreateDesignator(ctx, true);
    if PeekReservedWord(ctx, rwAssign) then
        CreateDesignatorStatement := TAssignmentStatement.Create(ctx, varRef)
    else if varRef is TCall then
        CreateDesignatorStatement := varRef
    else
        CreateDesignatorStatement := TCall.Create(ctx, varRef);
end;

function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;
begin
    CreateStatement := nil;
    case nextTokenKind.primitiveKind of
        pkIdentifier: CreateStatement := CreateDesignatorStatement(ctx);
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwInherited: CreateStatement := CreateDesignatorStatement(ctx);
                rwWith: CreateStatement := TWithStatement.Create(ctx);
                rwFor: CreateStatement := TForStatement.Create(ctx);
                rwCase: CreateStatement := TCaseStatement.Create(ctx);
                rwIf: CreateStatement := TIfStatement.Create(ctx);
                rwWhile: CreateStatement := TWhileStatement.Create(ctx);
                rwRepeat: CreateStatement := TRepeatStatement.Create(ctx);
                rwTry: CreateStatement := TTryStatement.Create(ctx);
                rwGoto: exit(nil); // TODO: CreateStatement := TGotoStatement.Create(ctx);
                rwBegin: CreateStatement := CreateCompoundStatement(ctx);
            end;
    end;
end;

end.
