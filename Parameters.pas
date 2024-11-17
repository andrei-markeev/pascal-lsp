unit Parameters;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDefs;

type
    TParameterKind = (ptkValue, ptkConst, ptkVar, ptkOut, ptkUntyped);

    TParameter = record
        kind: TParameterKind;
        name: shortstring;
        typeDef: PTypeDef;
    end;

    TParameterDynArray = array of TParameter;

    TParameterList = class
    public
        count: integer;
        items: array of TParameter;
        constructor Create;
        constructor Create(initialItems: TParameterDynArray);
        procedure Add(item: TParameter);
        destructor Destroy; override;
    end;

function CreateParam(kind: TParameterKind; name: shortstring; typeDef: PTypeDef): TParameter;

implementation

function CreateParam(kind: TParameterKind; name: shortstring; typeDef: PTypeDef): TParameter;
begin
    CreateParam.kind := kind;
    CreateParam.name := name;
    CreateParam.typeDef := typeDef;
end;

constructor TParameterList.Create;
begin
    SetLength(items, 0);
end;

constructor TParameterList.Create(initialItems: TParameterDynArray);
begin
    items := initialItems;
    count := length(initialItems);
end;

procedure TParameterList.Add(item: TParameter);
var
    l: integer;
begin
    l := length(items);
    count := l + 1;
    SetLength(items, l + 1);
    items[l] := item;
end;

destructor TParameterList.Destroy;
var
    i: integer;
begin
    SetLength(items, 0);
end;

end.
