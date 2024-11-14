program Constructors;

{$mode objfpc}

type
    TTestClass = class
        someVar: string;
        constructor Create;
        constructor TestSelf;
    end;

constructor TTestClass.Create;
begin
    someVar := 'Hello world!';
end;

constructor TTestClass.TestSelf;
begin
    Self.someVar := '2222222';
    Self.nonExistingField := '111111';
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