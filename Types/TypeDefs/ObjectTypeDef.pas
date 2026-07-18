unit ObjectTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TObjectTypeDef = class(TTypeDef)
    public
        objectFields: TFPHashList;
        parentObject: TTypeDef;
        constructor Create(AObjectFields: TFPHashList = nil; AParentObject: TTypeDef = nil; ASize: longword = 0);
    end;

implementation

constructor TObjectTypeDef.Create(AObjectFields: TFPHashList; AParentObject: TTypeDef; ASize: longword);
begin
    inherited Create(tkObject, ASize);
    if AObjectFields <> nil then
        objectFields := AObjectFields
    else
        objectFields := TFPHashList.Create;
    parentObject := AParentObject;
end;

end.
