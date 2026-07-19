unit RecordTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TRecordTypeDef = class(TTypeDef)
    public
        recordFields: TFPHashList;
        constructor Create(ctx: TObject = nil);
        destructor Destroy; override;
    end;

implementation

constructor TRecordTypeDef.Create(ctx: TObject);
begin
    inherited Create(ctx, tkRecord, 0);
    recordFields := TFPHashList.Create;
end;

destructor TRecordTypeDef.Destroy;
begin
    recordFields.Free;
    inherited Destroy;
end;

end.
