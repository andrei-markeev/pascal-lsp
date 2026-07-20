unit ClassTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TClassTypeDef = class(TTypeDef)
    public
        classFields: TFPHashList;
        parentClass: TTypeDef;
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
    end;

implementation    

constructor TClassTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkClass, 0);
    classFields := TFPHashList.Create;
end;

destructor TClassTypeDef.Destroy;
begin
    classFields.Free;
end;

end.
