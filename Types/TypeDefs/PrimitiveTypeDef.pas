unit PrimitiveTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TPrimitiveTypeDef = class(TTypeDef)
    public
        constructor Create(ctx: TTypeDefTracker = nil; AKind: TTypeKind = tkUnknown; ASize: longword = 0);
    end;

implementation

constructor TPrimitiveTypeDef.Create(ctx: TTypeDefTracker; AKind: TTypeKind; ASize: longword);
begin
    inherited Create(ctx, AKind, ASize);
end;

end.
