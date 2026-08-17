program BadAnonymousRecordFieldAccess;

var
    rec: record
        x: integer;
    end;

begin
    rec.invalidField := 10;
end.
