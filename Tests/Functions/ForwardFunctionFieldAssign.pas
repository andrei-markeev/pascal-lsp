program ForwardFunctionFieldAssign;

{$mode objfpc}

type
    TRec = record
        kind: integer;
        name: string;
    end;

    TMyClass = class
        constructor Create;
        function TestMethod(k: integer): TRec;
    end;

constructor TMyClass.Create;
begin
end;

function TestForward(k: integer): TRec; forward;

function TestForward(k: integer): TRec;
begin
    TestForward.kind := k;
    TestForward.name := 'test';
end;

function TMyClass.TestMethod(k: integer): TRec;
begin
    TestMethod.kind := k;
    TestMethod.name := 'test';
end;

var
    obj: TMyClass;
    r1, r2: TRec;

begin
    obj := TMyClass.Create;
    r1 := TestForward(10);
    r2 := obj.TestMethod(20);
end.
