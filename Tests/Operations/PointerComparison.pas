program PointerComparison;
var
    p1, p2: PChar;
    ptr1, ptr2: Pointer;
    b: boolean;
begin
    b := p1 <= p2;
    b := p1 < p2;
    b := p1 >= p2;
    b := p1 > p2;

    b := ptr1 = ptr2;
    b := ptr1 <> ptr2;
    b := p1 = ptr1;
    b := ptr1 = nil;

    b := ptr1 <= ptr2;
end.
