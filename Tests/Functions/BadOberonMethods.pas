program BadOberonMethods;
{$mode universalpascal}

type
    TPoint = record
        x: integer;
        y: integer;
    end;

procedure (var self: TPoint) Move(dx, dy: integer);
begin
    self.x := self.x + dx;
    self.y := self.y + dy;
end;

procedure (var X: TPoint) TestImplicit;
begin
    y := 10;
end;

var
    pt: TPoint;
begin
    pt.Move(10);
    pt.NonExistingMethod(10);
end.
