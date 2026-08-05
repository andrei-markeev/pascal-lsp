unit Symbols;

{$mode objfpc}
{$longstrings on}

interface

uses
    math, contnrs, Token, Identifier, TypeDefs, TypeDef, ParserContext;

type
    TSymbolKind = (skUnknown, skConstant, skTypedConstant, skTypeName, skVariable, skProcedure, skFunction, skConstructor, skDestructor, skUnitName);
    TSymbol = class
    public
        kind: TSymbolKind;
        name: shortstring;
        displayName: shortstring;
        rangeToken: TToken;
        implRangeToken: TToken;
        uniquePrefix: shortstring;
        parent: TSymbol;
        declaration: TIdentifier;
        implementationDecl: TIdentifier;
        typeDef: TTypeDef;
        references: array of TIdentifier;
        children: array of TSymbol;
        isParameter: boolean;
        constructor Create;
        destructor Destroy; override;
        procedure AddReference(ident: TIdentifier);
        function GetCurrentReturnType(ctx: TParserContext): TTypeDef;
    end;

    TTryAddOverrideResult = (ovNotApplicable, ovNotFound, ovExactDuplicate, ovAdded);

const
    NUM_OF_SYMBOL_KINDS = 7;
    SymbolKindStr: array [0..NUM_OF_SYMBOL_KINDS-1] of shortstring = (
        '', 'constant', 'typed constant', 'type', 'variable', 'procedure', 'function'
    );

function TryAddOverride(ident: TIdentifier; symbolType: TTypeDef; cursor: PChar; symbolParent: TSymbol = nil): TTryAddOverrideResult;
function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: TTypeDef; cursor: PChar): TSymbol;
function RegisterSymbolByName(symbolName: string; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: TTypeDef; cursor: PChar): TSymbol;
function FindSymbol(findName: shortstring; cursor: PChar): TSymbol;
function FindSymbol(parent: TSymbol; findName: shortstring; cursor: PChar): TSymbol;
function FindSymbol(ident: TIdentifier): TSymbol;
function FindInheritedMemberSymbol(parentType: TTypeDef; findName: shortstring; cursor: PChar): TSymbol;
function IsSameOrSubclass(currentClass, targetClass: TTypeDef): boolean;
function IsMemberAccessible(accessCtx: TParserContext; targetClass: TTypeDef; memberVisibility: TVisibility; cursor: PChar; memberSymbol: TSymbol = nil): boolean;

implementation

uses
    sysutils, classes, Scopes, RoutineTypeDef, ClassTypeDef, ObjectTypeDef, PointerTypeDef;

var
    lastId: longword = 0;


function TryAddOverride(ident: TIdentifier; symbolType: TTypeDef; cursor: PChar; symbolParent: TSymbol): TTryAddOverrideResult;
var
    overloadedSymbol: TSymbol;
    overloads: TFPList;
    i: integer;
    matchedTypeDef: TRoutineTypeDef;
    tokenNameLen: integer;
begin
    if (symbolType = nil) or not (symbolType.kind in [tkProcedure, tkFunction]) then
        exit(ovNotApplicable);

    if symbolParent <> nil then
        overloadedSymbol := FindSymbol(symbolParent, ident.GetStr(), cursor)
    else
        overloadedSymbol := FindSymbol(ident.GetStr(), cursor);
    if overloadedSymbol = nil then
        exit(ovNotFound);

    if overloadedSymbol.typeDef is TRoutineTypeDef then
        overloads := TRoutineTypeDef(overloadedSymbol.typeDef).overloads
    else
        overloads := nil;

    matchedTypeDef := nil;
    if (overloadedSymbol.typeDef is TRoutineTypeDef) and HaveSameSignature(symbolType, overloadedSymbol.typeDef) then
        matchedTypeDef := TRoutineTypeDef(overloadedSymbol.typeDef);

    if (matchedTypeDef = nil) and (overloads <> nil) then
        for i := 0 to overloads.Count - 1 do
            if (TTypeDef(overloads.Items[i]) is TRoutineTypeDef) and HaveSameSignature(symbolType, TTypeDef(overloads.Items[i])) then
            begin
                matchedTypeDef := TRoutineTypeDef(overloads.Items[i]);
                break;
            end;

    if matchedTypeDef <> nil then
    begin
        if matchedTypeDef.rangeToken <> nil then
        begin
            tokenNameLen := length(matchedTypeDef.rangeToken.tokenName);
            if (tokenNameLen >= 4) and (Copy(matchedTypeDef.rangeToken.tokenName, tokenNameLen - 3, 4) = 'Decl') then
            begin
                overloadedSymbol.AddReference(ident);
                exit(ovAdded);
            end;
        end;
        exit(ovExactDuplicate);
    end;

    if (overloads = nil) and (overloadedSymbol.typeDef is TRoutineTypeDef) then
    begin
        overloads := TFPList.Create;
        TRoutineTypeDef(overloadedSymbol.typeDef).overloads := overloads;
    end;

    if overloads <> nil then
        overloads.Add(symbolType);
    overloadedSymbol.AddReference(ident);

    TryAddOverride := ovAdded;
end;

function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: TTypeDef; cursor: PChar): TSymbol;
var
    symbolName: shortstring;
begin
    if declaredAt.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    if length(declaredAt.name) > 0 then
        symbolName := declaredAt.name
    else
        SetString(symbolName, declaredAt.start, Min(255, declaredAt.len));

    RegisterSymbol := RegisterSymbolByName(symbolName, symbolParent, symbolKind, symbolType, cursor);

    with RegisterSymbol do
    begin
        declaration := declaredAt;
        SetLength(references, 1);
        references[0] := declaredAt;
        declaration.symbol := RegisterSymbol;
        declaration.name := symbolName;
        declaration.typeDef := symbolType;
        declaration.tokenName := 'SymbDecl';
    end;

end;

function RegisterSymbolByName(symbolName: string; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: TTypeDef; cursor: PChar): TSymbol;
var
    parentChildrenCount: integer;
begin

    RegisterSymbolByName := TSymbol.Create;
    with RegisterSymbolByName do
    begin
        typeDef := symbolType;
        if (symbolKind = skTypeName) and (symbolType <> nil) and (symbolType <> unknownType) then
            symbolType.typeSymbol := RegisterSymbolByName;
        uniquePrefix := IntToStr(lastId) + '.';
        inc(lastId);
        kind := symbolKind;
        parent := symbolParent;
        displayName := symbolName;
        if symbolParent <> nil then
        begin
            name := symbolParent.uniquePrefix + symbolName;
            parentChildrenCount := length(symbolParent.children);
            SetLength(symbolParent.children, parentChildrenCount + 1);
            symbolParent.children[parentChildrenCount] := RegisterSymbolByName;
        end
        else
            name := symbolName;
    end;
    FindScope(cursor).symbolsList.Add(LowerCase(RegisterSymbolByName.name), RegisterSymbolByName);
end;

function FindSymbol(findName: shortstring; cursor: PChar): TSymbol;
var
    scope: TScope;
begin
    findName := LowerCase(findName);
    scope := FindScope(cursor);
    repeat
        FindSymbol := TSymbol(scope.symbolsList.Find(findName));
        scope := scope.parentScope;
    until (scope = nil) or (FindSymbol <> nil);
end;

function FindSymbol(parent: TSymbol; findName: shortstring; cursor: PChar): TSymbol;
begin
    FindSymbol := FindSymbol(parent.uniquePrefix + findName, cursor);
end;

function FindSymbol(ident: TIdentifier): TSymbol;
var
    name: shortstring;
begin
    if ident.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    SetString(name, ident.start, ident.len);
    FindSymbol := FindSymbol(name, ident.start);
end;

constructor TSymbol.Create;
begin
    isParameter := false;
end;

destructor TSymbol.Destroy;
begin
    declaration := nil;
    implementationDecl := nil;
    implRangeToken := nil;
    SetLength(references, 0);
    SetLength(children, 0);
end;

procedure TSymbol.AddReference(ident: TIdentifier);
var
    l: integer;
begin
    l := length(references);
    SetLength(references, l + 1);
    references[l] := ident;
    ident.symbol := Self;
    ident.name := name;
    ident.typeDef := typeDef;
    ident.tokenName := 'SymbRef';
end;

function TSymbol.GetCurrentReturnType(ctx: TParserContext): TTypeDef;
begin
    Result := nil;
    if (kind in [skProcedure, skFunction, skConstructor, skDestructor]) and
       (typeDef <> nil) and (typeDef is TRoutineTypeDef) then
    begin
        if (rangeToken <> nil) and (rangeToken.endMarker = nil) and (rangeToken.start <= ctx.Cursor) then
            Result := TRoutineTypeDef(typeDef).returnType
        else if (implRangeToken <> nil) and (implRangeToken.endMarker = nil) and (implRangeToken.start <= ctx.Cursor) then
            Result := TRoutineTypeDef(typeDef).returnType;
    end;
end;

function IsSameOrSubclass(currentClass, targetClass: TTypeDef): boolean;
var
    c: TTypeDef;
begin
    if (currentClass = nil) or (targetClass = nil) then exit(false);
    c := currentClass;
    if (c.kind = tkPointer) and (c is TPointerTypeDef) and (TPointerTypeDef(c).pointerToType <> nil) then
        c := TPointerTypeDef(c).pointerToType;
    while c <> nil do
    begin
        if c = targetClass then exit(true);
        if (c.kind = tkClass) and (c is TClassTypeDef) then
            c := TClassTypeDef(c).parentClass
        else if (c.kind = tkObject) and (c is TObjectTypeDef) then
            c := TObjectTypeDef(c).parentObject
        else
            break;
    end;
    Result := false;
end;

function IsMemberAccessible(accessCtx: TParserContext; targetClass: TTypeDef; memberVisibility: TVisibility; cursor: PChar; memberSymbol: TSymbol = nil): boolean;
var
    declCtx: TParserContext;
    selfSym: TSymbol;
    currentClass: TTypeDef;
begin
    if not (memberVisibility in [vPrivate, vProtected]) then
        exit(true);

    if accessCtx = nil then
        accessCtx := FindContextForCursor(cursor);

    declCtx := nil;
    if (memberSymbol <> nil) and (memberSymbol.declaration <> nil) then
        declCtx := FindContextForCursor(memberSymbol.declaration.start)
    else if (targetClass <> nil) and (targetClass.typeSymbol <> nil) and (TSymbol(targetClass.typeSymbol).declaration <> nil) then
        declCtx := FindContextForCursor(TSymbol(targetClass.typeSymbol).declaration.start);

    if (accessCtx <> nil) and (declCtx <> nil) and (accessCtx = declCtx) then
        exit(true);

    if memberVisibility = vPrivate then
        exit(false);

    if memberVisibility = vProtected then
    begin
        selfSym := FindSymbol('self', cursor);
        if (selfSym <> nil) and (selfSym.typeDef <> nil) then
        begin
            currentClass := selfSym.typeDef;
            if IsSameOrSubclass(currentClass, targetClass) then
                exit(true);
        end;
    end;

    Result := false;
end;

function FindInheritedMemberSymbol(parentType: TTypeDef; findName: shortstring; cursor: PChar): TSymbol;
var
    curClass: TTypeDef;
    classSym: TSymbol;
    sym: TSymbol;
begin
    Result := nil;
    curClass := parentType;
    while curClass <> nil do
    begin
        classSym := TSymbol(curClass.typeSymbol);
        if classSym <> nil then
        begin
            sym := FindSymbol(classSym, findName, cursor);
            if sym <> nil then
                exit(sym);
        end;
        if (curClass.kind = tkClass) and (curClass is TClassTypeDef) then
            curClass := TClassTypeDef(curClass).parentClass
        else if (curClass.kind = tkObject) and (curClass is TObjectTypeDef) then
            curClass := TObjectTypeDef(curClass).parentObject
        else
            curClass := nil;
    end;
end;

end.
