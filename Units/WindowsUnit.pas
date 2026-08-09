unit WindowsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TWindowsUnit = class(TSystemUnit)
    private
        functionType_GetStdHandle: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode;

destructor TWindowsUnit.Destroy;
begin
    if loaded then
        functionType_GetStdHandle.Free;
    inherited Destroy;
end;

procedure TWindowsUnit.InitTypes;
begin
    functionType_GetStdHandle := CreateOneParamFunctionType('nStdHandle', longintType, longintType);
end;

procedure TWindowsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if (ctx.mode = cmTurboPascal) or (ctx.mode >= cmFreePascal) then
    begin
        RegisterSymbolByName('THandle', nil, skTypeName, longintType, ctx.Cursor);
        RegisterSymbolByName('GetStdHandle', nil, skFunction, functionType_GetStdHandle, ctx.Cursor);
        RegisterSymbolByName('STD_INPUT_HANDLE', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('STD_OUTPUT_HANDLE', nil, skConstant, longintType, ctx.Cursor);
    end;
end;

end.
