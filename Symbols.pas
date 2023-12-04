unit Symbols;

{$mode objfpc}
{$longstrings on}

interface

uses
    math, contnrs, Token, Identifier;

type
    TSymbolKind = (skUnknown, skConstant, skTypedConstant, skTypeName, skVariable, skSubroutine);
    TSymbol = class
    public
        kind: TSymbolKind;
        name: shortstring;
        scopeStart: PChar;
        scopeToken: TToken;
        declaration: TIdentifier;
        references: array of TIdentifier;
        constructor Create(symbolName: string; symbolKind: TSymbolKind; declaredAt: TIdentifier; scope: TToken; cursor: PChar);
        destructor Destroy; override;
        procedure AddReference(ident: TIdentifier);
    end;

var
    SymbolsList: TFPHashList;

function RegisterSymbol(declaredAt: TIdentifier; symbolKind: TSymbolKind; scopeToken: TToken; cursor: PChar): TSymbol;

implementation

function RegisterSymbol(declaredAt: TIdentifier; symbolKind: TSymbolKind; scopeToken: TToken; cursor: PChar): TSymbol;
var
    name: shortstring;
begin
    if declaredAt.len > 255 then
        WriteLn('ERROR: identifier of more than 255 symbols found! Only first 255 will be used for indexing.');

    SetString(name, declaredAt.start, Min(255, declaredAt.len));
    RegisterSymbol := TSymbol.Create(name, symbolKind, declaredAt, scopeToken, Cursor);
    SymbolsList.Add(name, RegisterSymbol);
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

constructor TSymbol.Create(symbolName: string; symbolKind: TSymbolKind; declaredAt: TIdentifier; scope: TToken; cursor: PChar);
begin
    name := symbolName;
    kind := symbolKind;
    declaration := declaredAt;
    SetLength(references, 1);
    references[0] := declaredAt;
    scopeStart := cursor;
    scopeToken := scope;
    declaration.symbol := Self;
    declaration.tokenName := 'SymbDecl';
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
    ident.tokenName := 'SymbRef';
end;

initialization
    SymbolsList := TFPHashList.Create;
finalization
    SymbolsList.Free;
end.
