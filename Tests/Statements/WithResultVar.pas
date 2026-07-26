program WithResultVar;

type
    TPoint = record
        x, y: integer;
    end;

function MakePoint(ax, ay: integer): TPoint;
begin
    with MakePoint do
    begin
        x := ax;
        y := ay;
    end;
end;

begin
end.
