program OptionalSemicolon;

var
    a, b: integer;

begin
    if a = b then
    begin
        inc(a, 10);
        inc(b, 10)
    end;

    repeat
        inc(a);
        inc(b)
    until a > 100;

    case a of
        1: inc(b);
        2: inc(b)
    else
        inc(a);
        inc(b)
    end;
end.
