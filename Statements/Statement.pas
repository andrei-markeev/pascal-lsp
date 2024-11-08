unit Statement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token;

function CreateStatement(ctx: TParserContext): TToken; inline;
function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;

implementation

uses
    Symbols, InvalidSymbol, ReservedWord, Identifier,
    AssignmentStatement, IfStatement, WithStatement, ForStatement,
    WhileStatement, RepeatStatement, Block;

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
                symbol := FindSymbol(identName, ctx.Cursor);
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
                rwWhile: CreateStatement := TWhileStatement.Create(ctx);
                rwRepeat: CreateStatement := TRepeatStatement.Create(ctx);
                rwGoto: exit(nil); // TODO: CreateStatement := TGotoStatement.Create(ctx);
                rwBegin: CreateStatement := CreateBlock(ctx);
            end;
    end;
end;

end.
