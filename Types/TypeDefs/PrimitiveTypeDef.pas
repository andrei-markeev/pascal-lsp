unit PrimitiveTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TPrimitiveTypeDef = class(TTypeDef)
    public
        constructor Create(AKind: TTypeKind; ASize: longword = 0);
    end;

implementation

constructor TPrimitiveTypeDef.Create(AKind: TTypeKind; ASize: longword);
begin
    inherited Create(AKind, ASize);
end;

end.
