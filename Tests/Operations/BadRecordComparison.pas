program BadRecordComparison;
type
    R = record x: integer; end;
var
    r1, r2: R;
    b: boolean;
begin
    b := r1 <= r2;
end.
