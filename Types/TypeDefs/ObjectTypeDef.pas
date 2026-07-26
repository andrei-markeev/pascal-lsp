unit ObjectTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef, StructuredTypeDef;

type
    TObjectTypeDef = class(TStructuredTypeDef)
    public
        parentObject: TTypeDef;
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
    end;

implementation

constructor TObjectTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkObject);
end;

destructor TObjectTypeDef.Destroy;
begin
    inherited Destroy;
end;

end.
