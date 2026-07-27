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
        members: array of string;
        constructor Create(ctx: TTypeDefTracker = nil; AEnumSpec: Pointer = nil);
        procedure AddMember(const AName: string);
    end;

implementation

constructor TEnumTypeDef.Create(ctx: TTypeDefTracker; AEnumSpec: Pointer);
begin
    inherited Create(ctx, tkEnum, 1);
    enumSpec := AEnumSpec;
end;

procedure TEnumTypeDef.AddMember(const AName: string);
var
    len: integer;
begin
    len := Length(members);
    SetLength(members, len + 1);
    members[len] := AName;
end;

end.
