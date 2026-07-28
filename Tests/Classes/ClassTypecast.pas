program ClassTypecast;

{$mode objfpc}

type
    TParentClass = class
        parentField: integer;
        constructor Create;
    end;

    TChildClass = class(TParentClass)
        childField: integer;
    end;

constructor TParentClass.Create;
begin
end;

function GetChild(p: TParentClass): TParentClass;
begin
    GetChild := p;
end;

var
    parentObj: TParentClass;
    childObj: TChildClass;
    otherChild: TChildClass;

begin
    parentObj := TParentClass.Create;
    childObj := TChildClass.Create;

    // Typecast child class variable to parent class
    parentObj := TParentClass(childObj);

    // Typecast parent class variable to child class
    otherChild := TChildClass(parentObj);

    // Typecast class to generic Pointer
    parentObj := TParentClass(Pointer(childObj));

    // Typecast function call in expression
    otherChild := TChildClass(GetChild(parentObj));

    // Typecast on LHS of assignment
    TChildClass(parentObj).childField := 42;
end.
