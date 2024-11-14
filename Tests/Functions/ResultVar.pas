program ResultVar;

{$mode objfpc}

function Add(a, b: integer): integer;
begin
    result := a + b;
end;

var x: integer;
begin
    x := Add(10, 20);
end.