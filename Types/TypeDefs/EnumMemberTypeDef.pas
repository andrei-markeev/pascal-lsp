unit EnumMemberTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TEnumMemberTypeDef = class(TTypeDef)
    public
        enumType: TTypeDef;
        enumSpec: Pointer;
        constructor Create(AEnumType: TTypeDef = nil; AEnumSpec: Pointer = nil);
    end;

implementation

constructor TEnumMemberTypeDef.Create(AEnumType: TTypeDef; AEnumSpec: Pointer);
begin
    inherited Create(tkEnumMember, 1);
    enumType := AEnumType;
    enumSpec := AEnumSpec;
end;

end.
