program BadSealedHeritage;

{$mode objfpc}

type
    TSealedParent = class sealed
        field1: integer;
    end;

    // Inheriting from sealed class is not allowed
    TInvalidChild = class(TSealedParent)
        field2: integer;
    end;

begin
end.
