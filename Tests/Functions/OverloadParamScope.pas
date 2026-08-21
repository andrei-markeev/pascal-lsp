unit OverloadParamScope;

{$mode objfpc}

interface

type
    TTestObj = class
        constructor Create(val: integer); overload;
        constructor Create(val: integer; name: string); overload;
    end;

implementation

constructor TTestObj.Create(val: integer);
begin
end;

constructor TTestObj.Create(val: integer; name: string);
var
    s: string;
begin
    s := name;
end;

end.
