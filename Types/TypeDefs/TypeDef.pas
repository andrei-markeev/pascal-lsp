unit TypeDef;

{$mode objfpc}
{$longstrings on}

interface

type
    TTypeKind = (
        tkUnknown, tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum, tkEnumMember,
        tkReal, tkString, tkPointer, tkArray, tkDynamicArray,
        tkRecord, tkObject, tkClass, tkSet, tkFile, tkProcedure, tkFunction, tkUnitName
    );

    TVisibility = (vPublic, vPrivate, vProtected, vUnknown);

    TTypeDef = class
    public
        size: longword;
        visibility: TVisibility;
        kind: TTypeKind;
        constructor Create(AKind: TTypeKind = tkUnknown; ASize: longword = 0; AVisibility: TVisibility = vPublic);
        function Clone: TTypeDef; virtual;
    end;

implementation

constructor TTypeDef.Create(AKind: TTypeKind; ASize: longword; AVisibility: TVisibility);
begin
    inherited Create;
    kind := AKind;
    size := ASize;
    visibility := AVisibility;
end;

function TTypeDef.Clone: TTypeDef;
begin
    Result := TTypeDef.Create(kind, size, visibility);
end;

end.
