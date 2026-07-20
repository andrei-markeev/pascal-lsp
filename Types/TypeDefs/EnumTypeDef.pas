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
        constructor Create(ctx: TTypeDefTracker = nil; AEnumSpec: Pointer = nil);
    end;

implementation

constructor TEnumTypeDef.Create(ctx: TTypeDefTracker; AEnumSpec: Pointer);
begin
    inherited Create(ctx, tkEnum, 1);
    enumSpec := AEnumSpec;
end;

end.
