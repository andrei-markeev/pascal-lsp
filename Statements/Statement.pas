unit Statement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, Token, InvalidSymbol, ReservedWord, Identifier,
    AssignmentStatement, IfStatement, WithStatement, ForStatement;

function CreateStatement(ctx: TParserContext): TToken; inline;
function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;

implementation

uses
    Block;

function CreateStatement(ctx: TParserContext): TToken; inline;
begin
    CreateStatement := CreateStatement(ctx, DetermineNextTokenKind(ctx));
end;

function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;
var
    identName: string;
    symbol: TSymbol;
begin
    case nextTokenKind.primitiveKind of
        pkIdentifier:
            begin
                // 1. VarRef -> always starts with identifier, can be TypeName or Variable
                // 2. ProcedureCall -> always starts with identifier, can be Procedure or Function name
                identName := PeekIdentifier(ctx);
                symbol := FindSymbol(identName);
                if (symbol <> nil) and (symbol.kind in [skProcedure, skFunction]) then
                    exit(nil) // TODO: CreateStatement := TProcedureCallStatement.Create(ctx)
                else
                    CreateStatement := TAssignmentStatement.Create(ctx);
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwWith: CreateStatement := TWithStatement.Create(ctx);
                rwFor: CreateStatement := TForStatement.Create(ctx);
                rwIf: CreateStatement := TIfStatement.Create(ctx);
                rwWhile: exit(nil); // TODO: CreateStatement := TWhileStatement.Create(ctx);
                rwRepeat: exit(nil); // TODO: CreateStatement := TRepeatStatement.Create(ctx);
                rwGoto: exit(nil); // TODO: CreateStatement := TGotoStatement.Create(ctx);
                rwBegin: CreateStatement := CreateBlock(ctx);
            end;
    end;
end;

end.
