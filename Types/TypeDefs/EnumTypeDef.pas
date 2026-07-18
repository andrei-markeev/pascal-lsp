unit EnumTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TEnumTypeDef = class(TTypeDef)
    public
        enumSpec: Pointer;
        constructor Create(AEnumSpec: Pointer = nil);
    end;

implementation

constructor TEnumTypeDef.Create(AEnumSpec: Pointer);
begin
    inherited Create(tkEnum, 1);
    enumSpec := AEnumSpec;
end;

end.
