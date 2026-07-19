unit ObjectTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TObjectTypeDef = class(TTypeDef)
    public
        objectFields: TFPHashList;
        parentObject: TTypeDef;
        constructor Create;
        destructor Destroy; override;
    end;

implementation

constructor TObjectTypeDef.Create;
begin
    inherited Create(tkObject, 0);
    objectFields := TFPHashList.Create;
end;

destructor TObjectTypeDef.Destroy;
begin
    objectFields.Free;
    inherited Destroy;
end;

end.
