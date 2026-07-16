program GoodCaseOtherwise;

var
    x: integer;
begin
    { otherwise is valid in FPC mode (default) }
    case x of
        1: x := 10;
        2..5: x := 20;
        otherwise x := 0;
    end;

    { both else and otherwise are valid in FPC }
    case x of
        1: x := 10;
        else x := 99;
    end;
end.
