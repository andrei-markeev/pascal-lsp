program ArrayConstructor;

{$mode objfpc}

type
    TMyRecord = record
        x: integer;
    end;
    TMyRecordDynArray = array of TMyRecord;

    TMyClass = class
    end;

procedure TestParams(const arr: array of integer; dynArr: TMyRecordDynArray);
begin
end;

var
    dynArrInt: array of integer;
    dynArrRecord: array of TMyRecord;
    dynArrClass: array of TMyClass;
    rec: TMyRecord;
    c: TMyClass;

begin
    dynArrInt := [1, 2, 3];
    dynArrRecord := [rec];
    dynArrClass := [c];
    dynArrInt := [];
    TestParams([1, 2, 3], [rec]);
end.
