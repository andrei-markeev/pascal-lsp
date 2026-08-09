program ClassModifiers;

{$mode objfpc}

type
    TAbstractBase = class abstract
        val: integer;
        constructor Create;
    end;

    TSealedChild = class sealed (TAbstractBase)
        childVal: integer;
    end;

    TAbstractSealedClass = class abstract sealed
    end;

constructor TAbstractBase.Create;
begin
    val := 1;
end;

var
    obj1: TAbstractBase;
    obj2: TSealedChild;

begin
    obj1 := TAbstractBase.Create;
    obj2 := TSealedChild.Create;
end.
