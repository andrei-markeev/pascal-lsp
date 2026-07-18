unit ArrayTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TArrayTypeDef = class(TTypeDef)
    public
        typeOfIndex: TTypeDef;
        typeOfValues: TTypeDef;
        constructor Create(ATypeOfIndex: TTypeDef = nil; ATypeOfValues: TTypeDef = nil);
    end;

implementation

constructor TArrayTypeDef.Create(ATypeOfIndex: TTypeDef; ATypeOfValues: TTypeDef);
begin
    inherited Create(tkArray, 0);
    typeOfIndex := ATypeOfIndex;
    typeOfValues := ATypeOfValues;
end;

end.
