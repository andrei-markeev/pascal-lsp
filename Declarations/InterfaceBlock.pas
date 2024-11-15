unit InterfaceBlock;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord;

type
    TInterfaceBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    CompilationMode, Scopes, Identifier, ConstSection, TypeSection, VarSection, FunctionDecl, CompoundStatement;

constructor TInterfaceBlock.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i: integer;
begin
    tokenName := 'InterfaceBlock';
    ctx.Add(Self);

    start := ctx.Cursor;

    AddAnchor(rwConst);
    AddAnchor(rwType);
    AddAnchor(rwVar);
    AddAnchor(rwProcedure);
    AddAnchor(rwFunction);
    AddAnchor(rwImplementation);
    AddAnchor(rwEnd);

    nextTokenKind := SkipUntilAnchor(ctx);
    while nextTokenKind.reservedWordKind in [rwConst, rwType, rwVar, rwProcedure, rwFunction] do
    begin
        case nextTokenKind.reservedWordKind of
            rwConst: TConstSection.Create(ctx);
            rwType: TTypeSection.Create(ctx);
            rwVar: TVarSection.Create(ctx);
            rwProcedure, rwFunction: TFunctionDecl.Create(ctx, nextTokenKind.reservedWordKind, []);
        end;
        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwConst);
    RemoveAnchor(rwType);
    RemoveAnchor(rwVar);
    RemoveAnchor(rwProcedure);
    RemoveAnchor(rwFunction);
    RemoveAnchor(rwImplementation);
    RemoveAnchor(rwEnd);

    ctx.MarkEndOfToken(Self);
end;

end.
