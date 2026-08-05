program InheritedCall;

{$mode objfpc}

type
    TParentClass = class
        parentVal: integer;
        constructor Create(aVal: integer);
        procedure DoSomething; virtual;
        function GetValue: integer; virtual;
    end;

    TChildClass = class(TParentClass)
        childVal: integer;
        constructor Create(aVal, bVal: integer);
        procedure DoSomething; override;
        function GetValue: integer; override;
    end;

constructor TParentClass.Create(aVal: integer);
begin
    parentVal := aVal;
end;

procedure TParentClass.DoSomething;
begin
end;

function TParentClass.GetValue: integer;
begin
    GetValue := parentVal;
end;

constructor TChildClass.Create(aVal, bVal: integer);
begin
    inherited Create(aVal);
    childVal := bVal;
end;

procedure TChildClass.DoSomething;
begin
    inherited DoSomething;
    inherited;
end;

function TChildClass.GetValue: integer;
begin
    GetValue := inherited GetValue + childVal;
    inherited TParentClass.DoSomething;
end;

var
    c: TChildClass;
    v: integer;

begin
    c := TChildClass.Create(10, 20);
    c.DoSomething();
    v := c.GetValue();
end.
