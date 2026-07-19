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
        constructor Create(ctx: TObject = nil; AEnumSpec: Pointer = nil);
    end;

implementation

constructor TEnumTypeDef.Create(ctx: TObject; AEnumSpec: Pointer);
begin
    inherited Create(ctx, tkEnum, 1);
    enumSpec := AEnumSpec;
end;

end.
