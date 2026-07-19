unit SysutilsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TSysutilsUnit = class(TSystemUnit)
    private
        functionType_String_String: TTypeDef;
        functionType_LongInt_String: TTypeDef;
        setTypeOfReplaceFlags: TTypeDef;
        functionType_StringReplace: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode, SetTypeDef;

destructor TSysutilsUnit.Destroy;
begin
    if loaded then
    begin
        functionType_String_String.Free;
        functionType_LongInt_String.Free;
        setTypeOfReplaceFlags.Free;
        functionType_StringReplace.Free;
    end;
    inherited Destroy;
end;

procedure TSysutilsUnit.InitTypes;
begin
    functionType_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);
    functionType_LongInt_String := CreateOneParamFunctionType('v', longintType, ansiString64Type);

    setTypeOfReplaceFlags := TSetTypeDef.Create(unknownType, 1);

    functionType_StringReplace := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, 's', ansiString64Type),
        CreateParam(ptkConst, 'oldpattern', ansiString64Type),
        CreateParam(ptkConst, 'newpattern', ansiString64Type),
        CreateParam(ptkValue, 'flags', setTypeOfReplaceFlags)
    ]), ansiString64Type);
end;

procedure TSysutilsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('LowerCase', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('IntToStr', nil, skFunction, functionType_LongInt_String, ctx.Cursor);
        RegisterSymbolByName('StringReplace', nil, skFunction, functionType_StringReplace, ctx.Cursor);
    end;
end;

end.
