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

begin
    // TODO: test := TTestClass.Create;
end.