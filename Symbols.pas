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
        scopeStart: PChar;
        scope: TToken;
        declaration: TIdentifier;
        typeDef: TTypeDef;
        references: array of TIdentifier;
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

function RegisterSymbol(declaredAt: TIdentifier; symbolKind: TSymbolKind; scopeToken: TToken; symbolType: TTypeDef; cursor: PChar): TSymbol;
function FindSymbol(findName: shortstring): TSymbol; inline;
function FindSymbol(ident: TIdentifier): TSymbol;

implementation

function RegisterSymbol(declaredAt: TIdentifier; symbolKind: TSymbolKind; scopeToken: TToken; symbolType: TTypeDef; cursor: PChar): TSymbol;
var
    symbolName: shortstring;
begin
    if declaredAt.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    SetString(symbolName, declaredAt.start, Min(255, declaredAt.len));
    RegisterSymbol := TSymbol.Create;
    with RegisterSymbol do
    begin
        name := symbolName;
        kind := symbolKind;
        declaration := declaredAt;
        SetLength(references, 1);
        references[0] := declaredAt;
        scopeStart := cursor;
        scope := scopeToken;
        typeDef := symbolType;
        declaration.symbol := RegisterSymbol;
        declaration.typeDef := symbolType;
        declaration.tokenName := 'SymbDecl';
    end;
    SymbolsList.Add(symbolName, RegisterSymbol);
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
    ident.typeDef := typeDef;
    ident.tokenName := 'SymbRef';
end;

initialization
    SymbolsList := TFPHashList.Create;
finalization
    SymbolsList.Free;
end.
