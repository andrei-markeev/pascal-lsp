program DefaultParams;

{$mode objfpc}

type
    TTest = class
        constructor Create(x: integer = 10; s: string = 'hello'; b: boolean = true; p: pointer = nil);
    end;

constructor TTest.Create(x: integer = 10; s: string = 'hello'; b: boolean = true; p: pointer = nil);
begin
end;

begin
end.
