unit BadInitializationBlock;

{$mode objfpc}

interface

var
    GCount: integer;

implementation

initialization
    GCount := ;
    GCount := 1;
finalization
    GCount := 2;
end.
