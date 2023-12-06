unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, Token, ReservedWord, Identifier, AssignmentStatement;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TBlock.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    identName: shortstring;
    symbol: TSymbol;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwBegin, false);

    AddAnchor(rwEnd);
    AddAnchor(rwWith);
    AddAnchor(rwFor);
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwGoto);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwIf, rwWhile, rwRepeat, rwGoto])
          or (nextTokenKind.primitiveKind = pkIdentifier)
    do
    begin
        case nextTokenKind.primitiveKind of
            pkIdentifier:
                begin
                    identName := PeekIdentifier(ctx);
                    symbol := FindSymbol(identName);
                    if symbol.kind in [skProcedure, skFunction] then
                    begin
                        break;//TProcedureCallStatement.Create(ctx)
                    end
                    else if symbol.kind = skVariable then
                    begin
                        TAssignmentStatement.Create(ctx, symbol);
                    end
                    else
                        break; // not a valid statement, exiting block
                end;
            pkUnknown:
                case nextTokenKind.reservedWordKind of
                    rwWith: break;//TWithStatement.Create(ctx);
                    rwFor: break;//TForStatement.Create(ctx);
                    rwIf: break;//TIfStatement.Create(ctx);
                    rwWhile: break;//TWhileStatement.Create(ctx);
                    rwRepeat: break;//TRepeatStatement.Create(ctx);
                    rwGoto: break;//TGotoStatement.Create(ctx);
                end;
        end;
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        TReservedWord.Create(ctx, rwSemiColon, false);

        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwWith);
    RemoveAnchor(rwFor);
    RemoveAnchor(rwIf);
    RemoveAnchor(rwWhile);
    RemoveAnchor(rwRepeat);
    RemoveAnchor(rwGoto);
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
