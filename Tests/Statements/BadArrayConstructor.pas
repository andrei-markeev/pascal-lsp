program BadArrayConstructor;

{$mode objfpc}

type
    TMyRecord = record
        x: integer;
    end;

var
    dynArrInt: array of integer;
    dynArrRecord: array of TMyRecord;
    rec: TMyRecord;

begin
    dynArrInt := [1, 'hello'];
    dynArrInt := [rec];
end.
