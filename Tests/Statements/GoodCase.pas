program GoodCase;

var
    x: integer;
    ch: char;
    kind: (red, green, blue);
begin
    case x of
        1: x := 10;
        2, 3: x := 20;
        4..10: x := 30;
        else x := 0;
    end;

    case ch of
        'a'..'z': x := 1;
        'A'..'Z': x := 2;
    end;

    case kind of
        red: ch := 'r';
        green, blue: ch := 'g';
    end;
end.
