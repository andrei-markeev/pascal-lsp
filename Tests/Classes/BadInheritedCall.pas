program BadInheritedCall;

{$mode objfpc}

type
    TParentClass = class
    private
        procedure PrivateParentMethod;
    public
        constructor Create(aVal: integer);
    end;

    TChildClass = class(TParentClass)
        constructor Create(aVal, bVal: integer);
        procedure CallPrivate;
    end;

    TUnrelatedClass = class
    end;

procedure TParentClass.PrivateParentMethod;
begin
end;

constructor TParentClass.Create(aVal: integer);
begin
end;

constructor TChildClass.Create(aVal, bVal: integer);
begin
    inherited NonExistentMethod;
    inherited TUnrelatedClass.Create;
end;

procedure TChildClass.CallPrivate;
begin
    inherited PrivateParentMethod;
end;

procedure StandaloneProc;
begin
    inherited;
end;

begin
end.
