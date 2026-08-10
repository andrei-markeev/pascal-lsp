program MethodEquivalence;

{$mode objfpc}

type
    TTestClass = class
        procedure DoWork(x: integer);
        function Calculate(a: string): integer;
        constructor Create;
    end;

procedure TTestClass.DoWork(x: integer);
begin
end;

function TTestClass.Calculate(a: string): integer;
begin
    Result := 42;
end;

constructor TTestClass.Create;
begin
end;

begin
end.
