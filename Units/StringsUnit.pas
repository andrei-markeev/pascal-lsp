unit StringsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TStringsUnit = class(TSystemUnit)
    private
        functionType_PChar_PChar_LongInt_LongInt: TTypeDef;
        functionType_PChar_LongInt: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode;

destructor TStringsUnit.Destroy;
begin
    if loaded then
    begin
        functionType_PChar_PChar_LongInt_LongInt.Free;
        functionType_PChar_LongInt.Free;
    end;
    inherited Destroy;
end;

procedure TStringsUnit.InitTypes;
begin
    functionType_PChar_PChar_LongInt_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 'str1', pcharType),
        CreateParam(ptkValue, 'str2', pcharType),
        CreateParam(ptkValue, 'len', longintType)
    ]), longintType);
    functionType_PChar_LongInt := CreateOneParamFunctionType('str', pcharType, longintType);
end;

procedure TStringsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('strlicomp', nil, skFunction, functionType_PChar_PChar_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('StrLen', nil, skFunction, functionType_PChar_LongInt, ctx.Cursor);
    end;
end;

end.
