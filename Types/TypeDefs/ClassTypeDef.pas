unit ClassTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TClassTypeDef = class(TTypeDef)
    public
        classFields: TFPHashList;
        parentClass: TTypeDef;
        constructor Create(AClassFields: TFPHashList = nil; AParentClass: TTypeDef = nil; ASize: longword = 0);
    end;

implementation

constructor TClassTypeDef.Create(AClassFields: TFPHashList; AParentClass: TTypeDef; ASize: longword);
begin
    inherited Create(tkClass, ASize);
    if AClassFields <> nil then
        classFields := AClassFields
    else
        classFields := TFPHashList.Create;
    parentClass := AParentClass;
end;

end.
