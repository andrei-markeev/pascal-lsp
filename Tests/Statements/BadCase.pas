program BadCase;

var
    x: integer;
begin
    { missing 'of' }
    case x
        1: x := 10;
    end;

    { missing ':' in branch }
    case x of
        2 x := 20;
    end;

    { missing 'end' }
    case x of
        3: x := 30;

end.
