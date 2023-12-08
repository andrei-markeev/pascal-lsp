program GoodFor;

var
    i, j: integer;
    s: string;
begin
    s := '';
    for i := 0 to 10 do
        for j := 2 downto 3 do
            s := s + 'O';
end.