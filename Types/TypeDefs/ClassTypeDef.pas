unit ClassTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef, StructuredTypeDef;

type
    TClassTypeDef = class(TStructuredTypeDef)
    public
        parentClass: TTypeDef;
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
    end;

implementation    

constructor TClassTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkClass, 8);
end;

destructor TClassTypeDef.Destroy;
begin
    inherited Destroy;
end;

end.
