unit RecordTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef, StructuredTypeDef;

type
    TRecordTypeDef = class(TStructuredTypeDef)
    public
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
    end;

implementation

constructor TRecordTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkRecord);
end;

destructor TRecordTypeDef.Destroy;
begin
    inherited Destroy;
end;

end.
