unit ImplementationBlock;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, TypeDefs, Token, ReservedWord;

type
    TImplementationBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Scopes, ConstSection, TypeSection, VarSection, FunctionImpl, CompoundStatement;

constructor TImplementationBlock.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    i: integer;
begin
    tokenName := 'ImplementationBlock';
    ctx.Add(Self);

    start := ctx.Cursor;

    AddAnchor(rwConst);
    AddAnchor(rwType);
    AddAnchor(rwVar);
    AddAnchor(rwProcedure);
    AddAnchor(rwFunction);
    AddAnchor(rwConstructor);
    AddAnchor(rwDestructor);
    AddAnchor(rwBegin);
    AddAnchor(rwEnd);

    nextTokenKind := SkipUntilAnchor(ctx);
    while nextTokenKind.reservedWordKind in [rwConst, rwType, rwVar, rwProcedure, rwFunction, rwConstructor, rwDestructor] do
    begin
        case nextTokenKind.reservedWordKind of
            rwConst: TConstSection.Create(ctx);
            rwType: TTypeSection.Create(ctx);
            rwVar: TVarSection.Create(ctx);
            rwProcedure, rwFunction, rwConstructor, rwDestructor: TFunctionImpl.Create(ctx);
        end;
        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwConst);
    RemoveAnchor(rwType);
    RemoveAnchor(rwVar);
    RemoveAnchor(rwProcedure);
    RemoveAnchor(rwFunction);
    RemoveAnchor(rwConstructor);
    RemoveAnchor(rwDestructor);
    RemoveAnchor(rwBegin);
    RemoveAnchor(rwEnd);

    if nextTokenKind.reservedWordKind = rwBegin then
        CreateCompoundStatement(ctx)
    else if nextTokenKind.reservedWordKind = rwInitialization then
    begin
        TCompoundStatement.Create(ctx, rwInitialization);
        nextTokenKind := DetermineNextTokenKind(ctx);
        if nextTokenKind.reservedWordKind = rwFinalization then
            TCompoundStatement.Create(ctx, rwFinalization);
    end
    else if nextTokenKind.reservedWordKind = rwFinalization then
        TCompoundStatement.Create(ctx, rwFinalization)
    else if nextTokenKind.reservedWordKind = rwEnd then
        TReservedWord.Create(ctx, rwEnd, true);

    ctx.MarkEndOfToken(Self);
end;

end.
