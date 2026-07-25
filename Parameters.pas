unit Parameters;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDefs, TypeDef;

type
    TParameterKind = (ptkValue, ptkConst, ptkVar, ptkOut, ptkUntyped);

    TParameter = record
        kind: TParameterKind;
        name: shortstring;
        typeDef: TTypeDef;
        hasDefaultValue: boolean;
    end;

    TParameterDynArray = array of TParameter;

    TParameterList = class
    public
        count: integer;
        items: array of TParameter;
        constructor Create;
        constructor Create(initialItems: TParameterDynArray);
        procedure Add(item: TParameter);
        function GetMinRequiredCount: integer;
        destructor Destroy; override;
    end;

function CreateParam(kind: TParameterKind; name: shortstring; typeDef: TTypeDef; hasDefault: boolean = false): TParameter;

implementation

function CreateParam(kind: TParameterKind; name: shortstring; typeDef: TTypeDef; hasDefault: boolean = false): TParameter;
begin
    CreateParam.kind := kind;
    CreateParam.name := name;
    CreateParam.typeDef := typeDef;
    CreateParam.hasDefaultValue := hasDefault;
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

function TParameterList.GetMinRequiredCount: integer;
begin
    Result := 0;
    while (Result < count) and not items[Result].hasDefaultValue do
        inc(Result);
end;

destructor TParameterList.Destroy;
begin
    SetLength(items, 0);
end;

end.
