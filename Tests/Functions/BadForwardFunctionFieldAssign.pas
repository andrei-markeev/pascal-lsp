program BadForwardFunctionFieldAssign;

{$mode objfpc}

type
    TRec = record
        kind: integer;
    end;

function TestForward(k: integer): TRec; forward

function TestForward(k: integer): TRec;
begin
    TestForward.kind := k;
end;

begin
end.
