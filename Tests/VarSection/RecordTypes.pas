program RecordTypes;

var
    p: record
        x, y: integer;
    end;

begin
    x := 123;
    p.x := 100;
    p.y := '19';
    p.z := 0;

    with p do
    begin
        x := 200;
        y := -1;
    end;
end.