program Classes;

{$mode objfpc}
{$longstrings on}

uses classes;

var
    hs: THandleStream;
    h: integer;
begin
    hs := THandleStream.Create(1);
    h := hs.Handle;
    hs.Free;
end.
