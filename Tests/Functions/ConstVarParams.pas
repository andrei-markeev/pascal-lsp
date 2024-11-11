program ConstVarParams;

procedure Sum(
    const x, y: integer;
    var z: integer
);
begin
    z := x + y;
    x := 10;
end;

var
    res: integer;
begin
    Sum(2, 2, res);
end.
