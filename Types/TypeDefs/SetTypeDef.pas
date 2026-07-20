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
        constructor Create(ctx: TTypeDefTracker = nil; ATypeOfSet: TTypeDef = nil; ASize: longword = 1);
    end;

implementation

constructor TSetTypeDef.Create(ctx: TTypeDefTracker; ATypeOfSet: TTypeDef; ASize: longword);
begin
    inherited Create(ctx, tkSet, ASize);
    typeOfSet := ATypeOfSet;
end;

end.
