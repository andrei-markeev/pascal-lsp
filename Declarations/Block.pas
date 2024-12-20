unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: PTypeDef; resultType: PTypeDef);
    end;

implementation

uses
    CompilationMode, Scopes, ConstSection, TypeSection, VarSection, FunctionImpl, CompoundStatement;

constructor TBlock.Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: PTypeDef; resultType: PTypeDef);
var
    nextTokenKind: TTokenKind;
    i: integer;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    RegisterScope(Self);

    if selfType <> nil then
        RegisterSymbolByName('Self', nil, skVariable, selfType, start);

    if (resultType <> nil) and (ctx.mode >= cmObjectFreePascal) then
        RegisterSymbolByName('Result', nil, skVariable, resultType, start);

    for i := 0 to length(childSymbols) - 1 do
        RegisterSymbol(childSymbols[i].declaration, nil, childSymbols[i].kind, childSymbols[i].typeDef, start);

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

    CreateCompoundStatement(ctx);

    ctx.MarkEndOfToken(Self);

    state := tsInvisible;
    endMarker.state := tsInvisible;
end;

end.
