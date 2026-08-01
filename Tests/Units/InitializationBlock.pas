unit InitializationBlock;

{$mode objfpc}

interface

var
    GCount: integer;

procedure InitHelper;

implementation

procedure InitHelper;
begin
    GCount := GCount + 1;
end;

initialization
    GCount := 0;
    InitHelper;
finalization
    GCount := -1;
    InitHelper;
end.
