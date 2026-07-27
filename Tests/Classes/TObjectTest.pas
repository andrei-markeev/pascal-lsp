program TObjectTest;

{$mode objfpc}

type
    TCustomObj = class(TObject)
        field: integer;
    end;

var
    obj: TObject;
    custom: TCustomObj;
begin
    obj := TObject.Create;
    obj.Free;
    custom := TCustomObj.Create;
    custom.Free;
end.
