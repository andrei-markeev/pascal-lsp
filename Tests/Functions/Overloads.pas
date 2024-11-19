program Overloads;

function Test: integer;
begin
    Test := 1;
end;

function Test: integer;
begin
    Test := 2;
end;

function Test(x: integer): integer;
begin
    Test := x * 2;
end;

var r1, r2, r3: integer;
begin
    r1 := Test;
    r2 := Test();
    r3 := Test(3);
end.