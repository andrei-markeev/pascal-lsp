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
        constructor Create(ctx: TObject = nil; ATypeOfSet: TTypeDef = nil; ASize: longword = 1);
    end;

implementation

constructor TSetTypeDef.Create(ctx: TObject; ATypeOfSet: TTypeDef; ASize: longword);
begin
    inherited Create(ctx, tkSet, ASize);
    typeOfSet := ATypeOfSet;
end;

end.
