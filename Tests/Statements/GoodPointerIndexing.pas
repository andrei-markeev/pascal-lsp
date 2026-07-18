{$mode objfpc}
program GoodPointerIndexing;

var
    p: PChar;
    s: string;
    ch: char;

begin
    ch := p[0];
    ch := s[1];
end.
