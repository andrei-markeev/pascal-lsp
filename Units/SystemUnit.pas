unit SystemUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDefs, Parameters;

type
    TSystemUnit = class
    protected
        loaded: boolean;
        procedure InitTypes; virtual;
    public
        constructor Create;
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); virtual;
    end;

function CreateFunctionType(params: TParameterList; returnType: TTypeDef): TTypeDef;
function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: TTypeDef): TTypeDef;
function CreateTwoParamFunctionType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef; returnType: TTypeDef): TTypeDef;
function CreateTwoParamVarFunctionType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef; returnType: TTypeDef): TTypeDef;
function CreateProcedureType(params: TParameterList): TTypeDef;
function CreateOneParamProcedureType(paramName: shortstring; paramType: TTypeDef): TTypeDef;
function CreateTwoParamProcedureType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef): TTypeDef;

implementation

uses
    TypeDef, RoutineTypeDef;

constructor TSystemUnit.Create;
begin
    loaded := false;
end;

destructor TSystemUnit.Destroy;
begin
    inherited Destroy;
end;

procedure TSystemUnit.InitTypes;
begin
end;

procedure TSystemUnit.Load(ctx: TParserContext);
begin
    if not loaded then
    begin
        InitTypes;
        loaded := true;
    end;
end;

function CreateFunctionType(params: TParameterList; returnType: TTypeDef): TTypeDef;
begin
    CreateFunctionType := TRoutineTypeDef.Create(nil, tkFunction, params, returnType, nil);
end;

function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: TTypeDef): TTypeDef;
begin
    CreateOneParamFunctionType := CreateFunctionType(TParameterList.Create([CreateParam(ptkValue, paramName, paramType)]), returnType);
end;

function CreateTwoParamFunctionType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef; returnType: TTypeDef): TTypeDef;
begin
    CreateTwoParamFunctionType := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, param1Name, param1Type),
        CreateParam(ptkValue, param2Name, param2Type)
    ]), returnType);
end;

function CreateTwoParamVarFunctionType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef; returnType: TTypeDef): TTypeDef;
begin
    CreateTwoParamVarFunctionType := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, param1Name, param1Type),
        CreateParam(ptkVar, param2Name, param2Type)
    ]), returnType);
end;

function CreateProcedureType(params: TParameterList): TTypeDef;
begin
    CreateProcedureType := TRoutineTypeDef.Create(nil, tkProcedure, params, nil, nil);
end;

function CreateOneParamProcedureType(paramName: shortstring; paramType: TTypeDef): TTypeDef;
begin
    CreateOneParamProcedureType := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, paramName, paramType)
    ]));
end;

function CreateTwoParamProcedureType(param1Name: shortstring; param1Type: TTypeDef; param2Name: shortstring; param2Type: TTypeDef): TTypeDef;
begin
    CreateTwoParamProcedureType := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, param1Name, param1Type),
        CreateParam(ptkValue, param2Name, param2Type)
    ]));
end;

end.
