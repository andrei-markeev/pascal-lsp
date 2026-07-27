program BadResultVar;

{$mode objfpc}

function Add(a, b: integer): integer;
begin
    Add := 'invalid string';
    Add.nonExistentField := 123;
end;

begin
end.
