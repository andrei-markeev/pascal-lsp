program AnonymousRecordFieldAccess;

var
    rec: record
        x: integer;
        sub: record
            y: integer;
        end;
    end;

begin
    rec.x := 10;
    rec.sub.y := 20;
end.
