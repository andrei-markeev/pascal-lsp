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

    TTypeDefTracker = class
    public
        procedure TrackTypeDef(typeDef: TObject); virtual; abstract;
    end;

    TTypeDef = class
    public
        size: longword;
        visibility: TVisibility;
        kind: TTypeKind;
        constructor Create(ctx: TTypeDefTracker = nil; AKind: TTypeKind = tkUnknown; ASize: longword = 0; AVisibility: TVisibility = vPublic);
        function Clone: TTypeDef; virtual;
    end;

implementation

constructor TTypeDef.Create(ctx: TTypeDefTracker; AKind: TTypeKind; ASize: longword; AVisibility: TVisibility);
begin
    inherited Create;
    kind := AKind;
    size := ASize;
    visibility := AVisibility;
    if ctx <> nil then
        ctx.TrackTypeDef(Self);
end;

function TTypeDef.Clone: TTypeDef;
begin
    Result := TTypeDef.Create(nil, kind, size, visibility);
end;

end.
