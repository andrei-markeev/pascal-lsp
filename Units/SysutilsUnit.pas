unit SysutilsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TSysutilsUnit = class(TSystemUnit)
    private
        functionType_String_String: TTypeDef;
        functionType_LongInt_String: TTypeDef;
        typeReplaceFlag: TTypeDef;
        memberTypeOfReplaceFlag: TTypeDef;
        setTypeOfReplaceFlags: TTypeDef;
        functionType_StringReplace: TTypeDef;
        functionType_String: TTypeDef;
        functionType_String_Boolean: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode, SetTypeDef, EnumTypeDef, EnumMemberTypeDef;

destructor TSysutilsUnit.Destroy;
begin
    if loaded then
    begin
        functionType_String_String.Free;
        functionType_LongInt_String.Free;
        typeReplaceFlag.Free;
        memberTypeOfReplaceFlag.Free;
        setTypeOfReplaceFlags.Free;
        functionType_StringReplace.Free;
        functionType_String.Free;
        functionType_String_Boolean.Free;
    end;
    inherited Destroy;
end;

procedure TSysutilsUnit.InitTypes;
begin
    functionType_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);
    functionType_LongInt_String := CreateOneParamFunctionType('v', longintType, ansiString64Type);

    typeReplaceFlag := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(typeReplaceFlag).AddMember('rfReplaceAll');
    TEnumTypeDef(typeReplaceFlag).AddMember('rfIgnoreCase');

    memberTypeOfReplaceFlag := TEnumMemberTypeDef.Create(nil, typeReplaceFlag, nil);

    setTypeOfReplaceFlags := TSetTypeDef.Create(nil, typeReplaceFlag, 1);

    functionType_StringReplace := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, 's', ansiString64Type),
        CreateParam(ptkConst, 'oldpattern', ansiString64Type),
        CreateParam(ptkConst, 'newpattern', ansiString64Type),
        CreateParam(ptkValue, 'flags', setTypeOfReplaceFlags)
    ]), ansiString64Type);

    functionType_String := CreateFunctionType(TParameterList.Create, ansiString64Type);
    functionType_String_Boolean := CreateOneParamFunctionType('s', ansiString64Type, booleanType);
end;

procedure TSysutilsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TReplaceFlag', nil, skTypeName, typeReplaceFlag, ctx.Cursor);
        RegisterSymbolByName('rfReplaceAll', nil, skConstant, memberTypeOfReplaceFlag, ctx.Cursor);
        RegisterSymbolByName('rfIgnoreCase', nil, skConstant, memberTypeOfReplaceFlag, ctx.Cursor);
        RegisterSymbolByName('TReplaceFlags', nil, skTypeName, setTypeOfReplaceFlags, ctx.Cursor);

        RegisterSymbolByName('LowerCase', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('IntToStr', nil, skFunction, functionType_LongInt_String, ctx.Cursor);
        RegisterSymbolByName('StringReplace', nil, skFunction, functionType_StringReplace, ctx.Cursor);
        RegisterSymbolByName('GetCurrentDir', nil, skFunction, functionType_String, ctx.Cursor);
        RegisterSymbolByName('FileExists', nil, skFunction, functionType_String_Boolean, ctx.Cursor);
        RegisterSymbolByName('DirectoryExists', nil, skFunction, functionType_String_Boolean, ctx.Cursor);
    end;
end;

end.
