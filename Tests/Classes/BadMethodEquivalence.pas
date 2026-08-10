program BadMethodEquivalence;

{$mode objfpc}

type
    TTestClass = class
        procedure ProcKindMismatch;
        procedure ParamCountMismatch(a: integer);
        procedure ParamNameMismatch(x: integer);
        procedure ParamTypeMismatch(x: integer);
        function ReturnTypeMismatch: integer;
    end;

function TTestClass.ProcKindMismatch: integer;
begin
    Result := 0;
end;

procedure TTestClass.ParamCountMismatch(a: integer; b: string);
begin
end;

procedure TTestClass.ParamNameMismatch(y: integer);
begin
end;

procedure TTestClass.ParamTypeMismatch(x: string);
begin
end;

function TTestClass.ReturnTypeMismatch: string;
begin
    Result := '';
end;

begin
end.
