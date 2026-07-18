unit SetTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TSetTypeDef = class(TTypeDef)
    public
        typeOfSet: TTypeDef;
        constructor Create(ATypeOfSet: TTypeDef = nil; ASize: longword = 1);
    end;

implementation

constructor TSetTypeDef.Create(ATypeOfSet: TTypeDef; ASize: longword);
begin
    inherited Create(tkSet, ASize);
    typeOfSet := ATypeOfSet;
end;

end.
