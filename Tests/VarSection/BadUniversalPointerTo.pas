program BadUniversalPointerTo;

{$mode universalpascal}

type
    PBad = ^integer;

var
    p: pointer to integer;
    val: integer;

begin
    val := p^;
end.
