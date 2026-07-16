program Heritage;

{$mode objfpc}

type
    TParentClass = class
        parentField: integer;
        constructor Create;
        function GetParentField: integer;
    end;

    TChildClass = class(TParentClass)
        childField: integer;
        function GetChildField: integer;
    end;

constructor TParentClass.Create;
begin
end;

function TParentClass.GetParentField: integer;
begin
    GetParentField := parentField;
end;

function TChildClass.GetChildField: integer;
begin
    GetChildField := childField;
end;

var
    child: TChildClass;
    val1, val2: integer;

begin
    child := TChildClass.Create;
    child.parentField := 100;
    child.childField := 200;
    val1 := child.GetParentField();
    val2 := child.GetChildField();
end.
