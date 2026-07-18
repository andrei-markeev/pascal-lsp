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
    TypeDefs, TypeDef, TypedToken, ReservedWord, VarRef, Call,
    AssignmentStatement, CaseStatement, IfStatement, WithStatement, ForStatement,
    WhileStatement, RepeatStatement, CompoundStatement;

function CreateStatement(ctx: TParserContext): TToken;
begin
    CreateStatement := CreateStatement(ctx, DetermineNextTokenKind(ctx));
end;

function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;
var
    varRef: TTypedToken;
begin
    CreateStatement := nil;
    case nextTokenKind.primitiveKind of
        pkIdentifier:
            begin
                // This is either an assignment or a procedure call
                varRef := CreateVarRef(ctx);
                if (varRef <> nil) and (varRef.typeDef <> nil) and (varRef.typeDef.kind in [tkFunction, tkProcedure]) and not PeekReservedWord(ctx, rwAssign) then
                    CreateStatement := TCall.Create(ctx, varRef)
                else
                    CreateStatement := TAssignmentStatement.Create(ctx, varRef);
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwWith: CreateStatement := TWithStatement.Create(ctx);
                rwFor: CreateStatement := TForStatement.Create(ctx);
                rwCase: CreateStatement := TCaseStatement.Create(ctx);
                rwIf: CreateStatement := TIfStatement.Create(ctx);
                rwWhile: CreateStatement := TWhileStatement.Create(ctx);
                rwRepeat: CreateStatement := TRepeatStatement.Create(ctx);
                rwGoto: exit(nil); // TODO: CreateStatement := TGotoStatement.Create(ctx);
                rwBegin: CreateStatement := CreateCompoundStatement(ctx);
            end;
    end;
end;

end.
