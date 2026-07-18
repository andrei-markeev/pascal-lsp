unit StringsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TStringsUnit = class(TSystemUnit)
    private
        functionType_PChar_PChar_LongInt_LongInt: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode;

procedure TStringsUnit.InitTypes;
begin
    functionType_PChar_PChar_LongInt_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 'str1', @pcharType),
        CreateParam(ptkValue, 'str2', @pcharType),
        CreateParam(ptkValue, 'len', @longintType)
    ]), @longintType);
end;

procedure TStringsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('strlicomp', nil, skFunction, @functionType_PChar_PChar_LongInt_LongInt, ctx.Cursor);
    end;
end;

end.
