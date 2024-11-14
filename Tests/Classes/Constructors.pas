program Constructors;

{$mode objfpc}

type
    TTestClass = class
        someVar: string;
        constructor Create;
    end;

constructor TTestClass.Create;
begin
    someVar := 'Hello world!';
end;

var
    test: TTestClass;
    s: string;
    wrong: integer;

begin
    test := TTestClass.Create;
    s := test.someVar;
    wrong := test.someVar;
end.