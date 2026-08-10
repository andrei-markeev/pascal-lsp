unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDef, TypeDefs, Token, ReservedWord;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: TTypeDef; resultType: TTypeDef; funcImpl: TObject = nil);
    end;

implementation

uses
    CompilationMode, Scopes, ConstSection, TypeSection, VarSection, FunctionImpl, CompoundStatement, ClassTypeDef, ObjectTypeDef;

procedure RegisterInheritedMembers(selfType: TTypeDef; start: PChar);
var
    curClass: TTypeDef;
    classSym: TSymbol;
    childName: shortstring;
    i: integer;
    sym: TSymbol;
begin
    if selfType = nil then
        exit;

    if (selfType.kind = tkClass) and (selfType is TClassTypeDef) then
        curClass := TClassTypeDef(selfType).parentClass
    else if (selfType.kind = tkObject) and (selfType is TObjectTypeDef) then
        curClass := TObjectTypeDef(selfType).parentObject
    else
        curClass := nil;

    while curClass <> nil do
    begin
        classSym := TSymbol(curClass.typeSymbol);
        if classSym <> nil then
        begin
            for i := 0 to length(classSym.children) - 1 do
            begin
                childName := LowerCase(classSym.children[i].displayName);
                if FindScope(start).symbolsList.Find(childName) = nil then
                    RegisterSymbol(classSym.children[i], start);
            end;
        end;
        if (curClass.kind = tkClass) and (curClass is TClassTypeDef) then
            curClass := TClassTypeDef(curClass).parentClass
        else if (curClass.kind = tkObject) and (curClass is TObjectTypeDef) then
            curClass := TObjectTypeDef(curClass).parentObject
        else
            curClass := nil;
    end;
end;

constructor TBlock.Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: TTypeDef; resultType: TTypeDef; funcImpl: TObject = nil);
var
    nextTokenKind: TTokenKind;
    i: integer;
    sym: TSymbol;
begin
    tokenName := 'Block';
    ctx.Add(Self);

    start := ctx.Cursor;

    RegisterScope(Self);
    FindScope(Self.start).funcImpl := funcImpl;

    if selfType <> nil then
        RegisterSymbolByName('Self', nil, skVariable, selfType, start);

    if (resultType <> nil) and (mfFunctionResultVariable in Features[ctx.mode]) then
        RegisterSymbolByName('Result', nil, skVariable, resultType, start);

    for i := 0 to length(childSymbols) - 1 do
        RegisterSymbol(childSymbols[i], start);

    RegisterInheritedMembers(selfType, start);

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
