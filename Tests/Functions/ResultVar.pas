program ResultVar;

{$mode objfpc}

type
    TSymbol = record
        name: string;
    end;

function RegisterSymbolByName(name: string; id: integer): TSymbol;
var
    s: string;
begin
    RegisterSymbolByName.name := name;
    s := RegisterSymbolByName.name;
end;

function Add(a, b: integer): integer;
var
    temp: integer;
begin
    result := a + b;
    Add := result;
    temp := Add;
end;

var x: integer;
begin
    x := Add(10, 20);
end.