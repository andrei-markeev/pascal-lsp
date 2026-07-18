{$mode objfpc}
program BadPointerIndexing;

type
    PInteger = ^integer;

var
    p: PInteger;
    ptr: pointer;
    ch: char;

begin
    ch := p[0];
    ch := ptr[0];
end.
