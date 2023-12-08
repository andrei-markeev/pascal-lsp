unit Statement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, CommonFuncs, Token, InvalidSymbol, ReservedWord, Identifier,
    AssignmentStatement, IfStatement, WithStatement, ForStatement;

function CreateStatement(ctx: TParserContext): TToken; inline;
function CreateStatement(ctx: TParserContext; nextTokenKind: TTokenKind): TToken;

implementation

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
                identName := PeekIdentifier(ctx);
                symbol := FindSymbol(identName);
                if symbol = nil then
                begin
                    CreateStatement := TIdentifier.Create(ctx, true);
                    CreateStatement.state := tsMissing;
                end
                else if symbol.kind in [skProcedure, skFunction] then
                    exit(nil)//TProcedureCallStatement.Create(ctx)
                else if symbol.kind = skVariable then
                    CreateStatement := TAssignmentStatement.Create(ctx, symbol)
                else
                    CreateStatement := TInvalidSymbol.Create(ctx);
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwWith: TWithStatement.Create(ctx);
                rwFor: TForStatement.Create(ctx);
                rwIf: CreateStatement := TIfStatement.Create(ctx);
                rwWhile: exit(nil);//TWhileStatement.Create(ctx);
                rwRepeat: exit(nil);//TRepeatStatement.Create(ctx);
                rwGoto: exit(nil);//TGotoStatement.Create(ctx);
                rwBegin: CreateStatement := CommonFunctions.createBlock(ctx);
            end;
    end;
end;

end.
