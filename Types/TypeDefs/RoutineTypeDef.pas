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
        constructor Create(ctx: TObject = nil; AKind: TTypeKind = tkProcedure; AParameters: Pointer = nil; AReturnType: TTypeDef = nil; AOverloads: TFPList = nil);
        destructor Destroy; override;
    end;

implementation

uses
    Parameters;

constructor TRoutineTypeDef.Create(ctx: TObject; AKind: TTypeKind; AParameters: Pointer; AReturnType: TTypeDef; AOverloads: TFPList);
begin
    inherited Create(ctx, AKind, 0);
    parameters := AParameters;
    returnType := AReturnType;
    overloads := AOverloads;
end;

destructor TRoutineTypeDef.Destroy;
begin
    if parameters <> nil then
        TParameterList(parameters).Free;
    if overloads <> nil then
        overloads.Free;
    inherited Destroy;
end;

end.
