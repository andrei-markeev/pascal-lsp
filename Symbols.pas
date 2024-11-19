unit Symbols;

{$mode objfpc}
{$longstrings on}

interface

uses
    math, contnrs, Token, Identifier, TypeDefs;

type
    TSymbolKind = (skUnknown, skConstant, skTypedConstant, skTypeName, skVariable, skProcedure, skFunction, skConstructor, skDestructor, skUnitName);
    TSymbol = class
    public
        kind: TSymbolKind;
        name: shortstring;
        uniquePrefix: shortstring;
        parent: TSymbol;
        declaration: TIdentifier;
        typeDef: PTypeDef;
        references: array of TIdentifier;
        children: array of TSymbol;
        constructor Create;
        destructor Destroy; override;
        procedure AddReference(ident: TIdentifier);
    end;

    TTryAddOverrideResult = (ovNotApplicable, ovNotFound, ovExactDuplicate, ovAdded);

const
    NUM_OF_SYMBOL_KINDS = 7;
    SymbolKindStr: array [0..NUM_OF_SYMBOL_KINDS-1] of shortstring = (
        '', 'constant', 'typed constant', 'type', 'variable', 'procedure', 'function'
    );

function TryAddOverride(ident: TIdentifier; symbolType: PTypeDef; cursor: PChar): TTryAddOverrideResult;
function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: PTypeDef; cursor: PChar): TSymbol;
function RegisterSymbolByName(symbolName: string; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: PTypeDef; cursor: PChar): TSymbol;
function FindSymbol(findName: shortstring; cursor: PChar): TSymbol;
function FindSymbol(parent: TSymbol; findName: shortstring; cursor: PChar): TSymbol;
function FindSymbol(ident: TIdentifier): TSymbol;

implementation

uses
    sysutils, classes, Scopes;

var
    lastId: longword = 0;


function TryAddOverride(ident: TIdentifier; symbolType: PTypeDef; cursor: PChar): TTryAddOverrideResult;
var
    overloadedSymbol: TSymbol;
    overloads: TFPList;
    i: integer;
begin
    if not (symbolType^.kind in [tkProcedure, tkFunction]) then
        exit(ovNotApplicable);

    overloadedSymbol := FindSymbol(ident.GetStr(), cursor);
    if overloadedSymbol = nil then
        exit(ovNotFound);

    overloads := overloadedSymbol.typeDef^.overloads;

    if HaveSameSignature(symbolType, overloadedSymbol.typeDef) then
        exit(ovExactDuplicate);

    if overloads <> nil then
        for i := 0 to overloads.Count - 1 do
            if HaveSameSignature(symbolType, overloads.Items[i]) then
                exit(ovExactDuplicate);

    if overloads = nil then
    begin
        overloads := TFPList.Create;
        overloadedSymbol.typeDef^.overloads := overloads;
    end;

    overloads.Add(symbolType);
    overloadedSymbol.AddReference(ident);

    TryAddOverride := ovAdded;
end;

function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: PTypeDef; cursor: PChar): TSymbol;
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
        declaration.typeDef := symbolType^;
        declaration.tokenName := 'SymbDecl';
    end;

end;

function RegisterSymbolByName(symbolName: string; symbolParent: TSymbol; symbolKind: TSymbolKind; symbolType: PTypeDef; cursor: PChar): TSymbol;
var
    parentChildrenCount: integer;
begin

    RegisterSymbolByName := TSymbol.Create;
    with RegisterSymbolByName do
    begin
        typeDef := symbolType;
        uniquePrefix := IntToStr(lastId) + '.';
        inc(lastId);
        kind := symbolKind;
        parent := symbolParent;
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
end;

destructor TSymbol.Destroy;
begin
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
    ident.typeDef := typeDef^;
    ident.tokenName := 'SymbRef';
end;

end.
