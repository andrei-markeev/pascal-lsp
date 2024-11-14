program ValueParams;

function Sum(x: integer; y: integer): integer;
begin
    Sum := x + y;
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
    Sum(2, 2);
    Sum(123, 'hello');
    Sum('1');
    Sum;

    Sum
    (
        2,
        3,
        4
    );

    Sum

end.
