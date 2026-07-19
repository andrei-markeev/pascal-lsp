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
        constructor Create(ctx: TObject = nil; AEnumType: TTypeDef = nil; AEnumSpec: Pointer = nil);
    end;

implementation

constructor TEnumMemberTypeDef.Create(ctx: TObject; AEnumType: TTypeDef; AEnumSpec: Pointer);
begin
    inherited Create(ctx, tkEnumMember, 1);
    enumType := AEnumType;
    enumSpec := AEnumSpec;
end;

end.
