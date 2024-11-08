program GoodRepeat;

var
    i: integer;
    s: string;
begin
    i := 0;
    s := '';
    repeat
        i := i + 1;
        s := s + '1';
    until s = '11111';
end.