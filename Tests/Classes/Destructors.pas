program Destructors;

{$mode objfpc}

type
    TTestClass1 = class
        function Hello: string; virtual;
        destructor Test;
        destructor Destroy;
    end;
    TTestClass2 = class
        destructor Destroy; override;
    end;

function TTestClass1.Hello: string;
begin
    result := 'Hello there!';
end;

destructor TTestClass2.Destroy;
begin
end;

begin
end.