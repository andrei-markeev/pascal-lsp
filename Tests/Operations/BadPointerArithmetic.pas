program BadPointerArithmetic;
var
    p1, p2: PChar;
    ptr: Pointer;
    len: integer;
begin
    p1 := p1 + p2;
    p1 := 5 - p1;
    ptr := ptr + 1;
    ptr := ptr - 1;
    len := ptr - ptr;
end.
