program BadClassStaticMethods;

{$mode objfpc}

type
    TBadCalc = class
        class function Foo: integer static;
        class procedure Bar; static;
    end;

class function TBadCalc.Foo: integer; static;
begin
    Self := nil;
end;

class procedure TBadCalc.Bar; static;
begin
end;

begin
end.
