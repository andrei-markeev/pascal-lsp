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
        constructor Create(ctx: TObject = nil; ATypeOfDynValues: TTypeDef = nil; ASize: longword = 8);
    end;

implementation

constructor TDynamicArrayTypeDef.Create(ctx: TObject; ATypeOfDynValues: TTypeDef; ASize: longword);
begin
    inherited Create(ctx, tkDynamicArray, ASize);
    typeOfDynValues := ATypeOfDynValues;
end;

end.
