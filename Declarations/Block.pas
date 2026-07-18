unit Block;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord;

type
    TBlock = class(TToken)
    public
        constructor Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: TTypeDef; resultType: TTypeDef);
    end;

implementation

uses
    CompilationMode, Scopes, ConstSection, TypeSection, VarSection, FunctionImpl, CompoundStatement,
    TypeDef, ClassTypeDef, ObjectTypeDef;

function FindClassSymbol(typeDef: TTypeDef): TSymbol;
var
    i, j: integer;
    scope: TScope;
    sym: TSymbol;
begin
    Result := nil;
    for i := 0 to length(ScopesList) - 1 do
    begin
        scope := ScopesList[i];
        for j := 0 to scope.symbolsList.Count - 1 do
        begin
            sym := TSymbol(scope.symbolsList.Items[j]);
            if (sym <> nil) and (sym.kind = skTypeName) and (sym.typeDef = typeDef) then
            begin
                Result := sym;
                exit;
            end;
        end;
    end;
end;

procedure RegisterInheritedMembers(selfType: TTypeDef; start: PChar);
var
    currClass: TTypeDef;
    classSym: TSymbol;
    childName: shortstring;
    i: integer;
begin
    if selfType = nil then
        exit;

    if (selfType.kind = tkClass) and (selfType is TClassTypeDef) then
        currClass := TClassTypeDef(selfType).parentClass
    else if (selfType.kind = tkObject) and (selfType is TObjectTypeDef) then
        currClass := TObjectTypeDef(selfType).parentObject
    else
        currClass := nil;

    while currClass <> nil do
    begin
        classSym := FindClassSymbol(currClass);
        if classSym <> nil then
        begin
            for i := 0 to length(classSym.children) - 1 do
            begin
                childName := LowerCase(classSym.children[i].displayName);
                if FindScope(start).symbolsList.Find(childName) = nil then
                    RegisterSymbol(classSym.children[i].declaration, nil, classSym.children[i].kind, classSym.children[i].typeDef, start);
            end;
        end;
        if (currClass.kind = tkClass) and (currClass is TClassTypeDef) then
            currClass := TClassTypeDef(currClass).parentClass
        else if (currClass.kind = tkObject) and (currClass is TObjectTypeDef) then
            currClass := TObjectTypeDef(currClass).parentObject
        else
            currClass := nil;
    end;
end;

constructor TBlock.Create(ctx: TParserContext; childSymbols: array of TSymbol; selfType: TTypeDef; resultType: TTypeDef);
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
