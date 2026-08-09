program ClassStaticMethods;

{$mode objfpc}

type
    TCalculator = class
        class function Add(a, b: integer): integer; static;
        class procedure Reset; static;
    end;

class function TCalculator.Add(a, b: integer): integer; static;
begin
    Result := a + b;
end;

class procedure TCalculator.Reset; static;
begin
end;

var
    calc: TCalculator;
    res: integer;
begin
    res := TCalculator.Add(5, 10);
    TCalculator.Reset;

    res := calc.Add(1, 2);
    calc.Reset;
end.
