program GoodWhile;

var
    i, j: integer;
    a, b: string;
begin
    a := 'test';
    b := 'hello';
    i := 1;
    while a <> b do
    begin
        while i < 10 do i := i + 1;
        b := a;
    end;
end.