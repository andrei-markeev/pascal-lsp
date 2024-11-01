unit Symbols;

{$mode objfpc}
{$longstrings on}

interface

uses
    math, contnrs, Token, Identifier, TypeDefs;

type
    TSymbolKind = (skUnknown, skConstant, skTypedConstant, skTypeName, skVariable, skProcedure, skFunction, skUnitName);
    TSymbol = class
    public
        kind: TSymbolKind;
        name: shortstring;
        uniquePrefix: shortstring;
        parent: TSymbol;
        scopeStart: PChar;
        scope: TToken;
        declaration: TIdentifier;
        typeDef: TTypeDef;
        references: array of TIdentifier;
        children: array of TSymbol;
        constructor Create;
        destructor Destroy; override;
        procedure AddReference(ident: TIdentifier);
    end;

const
    NUM_OF_SYMBOL_KINDS = 7;
    SymbolKindStr: array [0..NUM_OF_SYMBOL_KINDS-1] of shortstring = (
        '', 'constant', 'typed constant', 'type', 'variable', 'procedure', 'function'
    );

var
    SymbolsList: TFPHashList;

procedure CleanupSymbols;
function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; scopeToken: TToken; symbolType: TTypeDef; cursor: PChar): TSymbol;
function FindSymbol(findName: shortstring): TSymbol; inline;
function FindSymbol(parent: TSymbol; findName: shortstring): TSymbol; inline;
function FindSymbol(ident: TIdentifier): TSymbol;

implementation

uses
    sysutils;

var
    lastId: longword = 0;

procedure CleanupSymbols;
begin
    lastId := 0;
    SymbolsList.Clear;
end;

function RegisterSymbol(declaredAt: TIdentifier; symbolParent: TSymbol; symbolKind: TSymbolKind; scopeToken: TToken; symbolType: TTypeDef; cursor: PChar): TSymbol;
var
    symbolName: shortstring;
    parentChildrenCount: integer;
begin
    if declaredAt.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    SetString(symbolName, declaredAt.start, Min(255, declaredAt.len));
    RegisterSymbol := TSymbol.Create;
    with RegisterSymbol do
    begin
        if symbolParent <> nil then
        begin
            name := symbolParent.uniquePrefix + symbolName;
            WriteLn('symbol with parent: ', name);
        end
        else
            name := symbolName;
        uniquePrefix := IntToStr(lastId) + '.';
        inc(lastId);
        kind := symbolKind;
        parent := symbolParent;
        if symbolParent <> nil then
        begin
            parentChildrenCount := length(symbolParent.children);
            SetLength(symbolParent.children, parentChildrenCount + 1);
            symbolParent.children[parentChildrenCount] := RegisterSymbol;
        end;
        declaration := declaredAt;
        SetLength(references, 1);
        references[0] := declaredAt;
        scopeStart := cursor;
        scope := scopeToken;
        typeDef := symbolType;
        declaration.symbol := RegisterSymbol;
        declaration.name := name;
        declaration.typeDef := symbolType;
        declaration.tokenName := 'SymbDecl';
    end;
    SymbolsList.Add(RegisterSymbol.name, RegisterSymbol);
end;

function FindSymbol(ident: TIdentifier): TSymbol;
var
    name: shortstring;
begin
    if ident.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    SetString(name, ident.start, Min(255, ident.len));
    FindSymbol := TSymbol(SymbolsList.Find(name));
end;

function FindSymbol(findName: shortstring): TSymbol; inline;
begin
    FindSymbol := TSymbol(SymbolsList.Find(findName));
end;

function FindSymbol(parent: TSymbol; findName: shortstring): TSymbol; inline;
begin
    FindSymbol := TSymbol(SymbolsList.Find(parent.uniquePrefix + findName));
end;

constructor TSymbol.Create;
begin
end;

destructor TSymbol.Destroy;
begin
    SetLength(references, 0);
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

initialization
    SymbolsList := TFPHashList.Create;
finalization
    SymbolsList.Free;
end.
