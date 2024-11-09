program ValueParams;

function Sum(x: integer; y: integer): integer;
var z: integer;
begin
    z := x + y;
end;

function BadSum(x, y: integer);
begin
    z := x + m;
end;

procedure Log(z: string);
begin
end;

procedure BadLog(z: string): string;
begin
end;

begin
end.
