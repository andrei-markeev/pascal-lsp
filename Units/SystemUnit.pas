unit SystemUnit;

{$mode objfpc}

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

function CreateFunctionType(params: TParameterList; returnType: PTypeDef): TTypeDef;
function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: PTypeDef): TTypeDef;
function CreateProcedureType(params: TParameterList): TTypeDef;

implementation

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

function CreateFunctionType(params: TParameterList; returnType: PTypeDef): TTypeDef;
begin
    CreateFunctionType.kind := tkFunction;
    CreateFunctionType.visibility := vPublic;
    CreateFunctionType.size := 0;
    CreateFunctionType.parameters := params;
    CreateFunctionType.returnType := returnType;
    CreateFunctionType.overloads := nil;
end;

function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: PTypeDef): TTypeDef;
begin
    CreateOneParamFunctionType := CreateFunctionType(TParameterList.Create([CreateParam(ptkValue, paramName, paramType)]), returnType);
end;

function CreateProcedureType(params: TParameterList): TTypeDef;
begin
    CreateProcedureType.kind := tkProcedure;
    CreateProcedureType.visibility := vPublic;
    CreateProcedureType.size := 0;
    CreateProcedureType.parameters := params;
    CreateProcedureType.overloads := nil;
end;

end.
