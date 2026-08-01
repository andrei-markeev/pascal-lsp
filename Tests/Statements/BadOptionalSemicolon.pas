program BadOptionalSemicolon;

var
    a, b: integer;

begin
    if a = b then
    begin
        inc(a, 10)
        inc(b, 10)
    end;
end.
