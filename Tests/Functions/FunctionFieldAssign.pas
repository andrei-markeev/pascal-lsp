program FunctionFieldAssign;

{$mode objfpc}

type
    TRec = record
        kind: integer;
        name: string;
    end;

function TestFunc(k: integer): TRec;
begin
    TestFunc.kind := k;
    TestFunc.name := 'test';
end;

var
    hello: array[1..100] of integer;

function TestFuncParamless: TRec;
begin
    TestFuncParamless.kind := 10;
    hello[TestFuncParamless.kind + 1] := 123;
end;

begin
    TestFunc(10);
    TestFuncParamless;
end.
