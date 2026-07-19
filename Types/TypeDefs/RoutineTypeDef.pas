unit RoutineTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    classes, TypeDef, Token;

type
    TRoutineTypeDef = class(TTypeDef)
    public
        parameters: Pointer;
        returnType: TTypeDef;
        overloads: TFPList;
        rangeToken: TToken;
        constructor Create(AKind: TTypeKind = tkProcedure; AParameters: Pointer = nil; AReturnType: TTypeDef = nil; AOverloads: TFPList = nil);
    end;

implementation

constructor TRoutineTypeDef.Create(AKind: TTypeKind; AParameters: Pointer; AReturnType: TTypeDef; AOverloads: TFPList);
begin
    inherited Create(AKind, 0);
    parameters := AParameters;
    returnType := AReturnType;
    overloads := AOverloads;
end;

end.
