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
        constructor Create(ARecordFields: TFPHashList = nil; ASize: longword = 0);
        destructor Destroy; override;
    end;

implementation

constructor TRecordTypeDef.Create(ARecordFields: TFPHashList; ASize: longword);
begin
    inherited Create(tkRecord, ASize);
    if ARecordFields <> nil then
        recordFields := ARecordFields
    else
        recordFields := TFPHashList.Create;
end;

destructor TRecordTypeDef.Destroy;
begin
    inherited Destroy;
end;

end.
