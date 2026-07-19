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
        constructor Create(ctx: TObject = nil; ATypeOfIndex: TTypeDef = nil; ATypeOfValues: TTypeDef = nil);
    end;

implementation

constructor TArrayTypeDef.Create(ctx: TObject; ATypeOfIndex: TTypeDef; ATypeOfValues: TTypeDef);
begin
    inherited Create(ctx, tkArray, 0);
    typeOfIndex := ATypeOfIndex;
    typeOfValues := ATypeOfValues;
end;

end.
