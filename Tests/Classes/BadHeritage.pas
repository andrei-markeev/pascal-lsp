program BadHeritage;

{$mode objfpc}

type
    TParentClass = class
        parentField: integer;
    end;

    // 1. Parent class does not exist
    TBadChild1 = class(TNonExisting)
        childField1: integer;
    end;

    // 2. Parent identifier is not a class
    TBadChild2 = class(integer)
        childField2: integer;
    end;

    // 3. Only open parenthesis
    TBadChild3 = class(
        childField3: integer;
    end;

    // 4. Missing closing parenthesis
    TBadChild4 = class(TParentClass
        childField4: integer;
    end;

    // 5. Circular inheritance (self)
    TBadChild5 = class(TBadChild5)
        childField5: integer;
    end;

begin
end.
