unit MathUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TMathUnit = class(TSystemUnit)
    private
        functionType_LongInt_LongInt_LongInt: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode;

procedure TMathUnit.InitTypes;
begin
    functionType_LongInt_LongInt_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 'a', longintType),
        CreateParam(ptkValue, 'b', longintType)
    ]), longintType);
end;

procedure TMathUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('Min', nil, skFunction, functionType_LongInt_LongInt_LongInt, ctx.Cursor);
    end;
end;

end.
