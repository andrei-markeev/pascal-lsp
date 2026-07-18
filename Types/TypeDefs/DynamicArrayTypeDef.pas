unit DynamicArrayTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TDynamicArrayTypeDef = class(TTypeDef)
    public
        typeOfDynValues: TTypeDef;
        constructor Create(ATypeOfDynValues: TTypeDef = nil; ASize: longword = 8);
    end;

implementation

constructor TDynamicArrayTypeDef.Create(ATypeOfDynValues: TTypeDef; ASize: longword);
begin
    inherited Create(tkDynamicArray, ASize);
    typeOfDynValues := ATypeOfDynValues;
end;

end.
