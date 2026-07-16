{$mode iso}
program BadCaseISO;

var
    x: integer;
    ch: char;
begin
    { range not supported in ISO 7185 }
    case x of
        1..5: x := 10;
    end;

    { else fallback not supported in ISO 7185 }
    case x of
        1: x := 10;
        else x := 0;
    end;

    { string labels not supported in ISO 7185 }
    case ch of
        'a': x := 1;
    end;
end.
