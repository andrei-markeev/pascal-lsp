unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, Token, ReservedWord, Statement;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Scopes, ConstSection, TypeSection, VarSection, FunctionImpl, CompoundStatement;

constructor TBlock.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    RegisterScope(Self);

    AddAnchor(rwConst);
    AddAnchor(rwType);
    AddAnchor(rwVar);
    AddAnchor(rwProcedure);
    AddAnchor(rwFunction);
    AddAnchor(rwBegin);
    AddAnchor(rwEnd);

    nextTokenKind := SkipUntilAnchor(ctx);
    while nextTokenKind.reservedWordKind in [rwConst, rwType, rwVar, rwProcedure, rwFunction] do
    begin
        case nextTokenKind.reservedWordKind of
            rwConst: TConstSection.Create(ctx);
            rwType: TTypeSection.Create(ctx);
            rwVar: TVarSection.Create(ctx);
            rwProcedure: TFunctionImpl.Create(ctx);
            rwFunction: TFunctionImpl.Create(ctx);
        end;
        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwConst);
    RemoveAnchor(rwType);
    RemoveAnchor(rwVar);
    RemoveAnchor(rwProcedure);
    RemoveAnchor(rwFunction);
    RemoveAnchor(rwBegin);
    RemoveAnchor(rwEnd);

    CreateCompoundStatement(ctx);

    ctx.MarkEndOfToken(Self);

    state := tsInvisible;
    endMarker.state := tsInvisible;
end;

end.
