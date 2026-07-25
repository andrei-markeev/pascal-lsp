program DefaultParams;

{$mode objfpc}

type
    TTest = class
        constructor Create(x: integer = 10; s: string = 'hello'; b: boolean = true; p: pointer = nil);
    end;

constructor TTest.Create(x: integer = 10; s: string = 'hello'; b: boolean = true; p: pointer = nil);
begin
end;

procedure SetVal(a: integer; b: integer = 100);
begin
end;

var
    t: TTest;
begin
    t := TTest.Create;
    t := TTest.Create(1);
    t := TTest.Create(1, 'a');
    t := TTest.Create(1, 'a', false);
    t := TTest.Create(1, 'a', false, nil);
    SetVal(10);
    SetVal(10, 20);
end.
