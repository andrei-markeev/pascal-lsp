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
        functionType_String_LongInt: TTypeDef;
        typeReplaceFlag: TTypeDef;
        memberTypeOfReplaceFlag: TTypeDef;
        setTypeOfReplaceFlags: TTypeDef;
        functionType_StringReplace: TTypeDef;
        functionType_String: TTypeDef;
        functionType_String_Boolean: TTypeDef;
        recordType_TSearchRec: TTypeDef;
        functionType_FindFirst: TTypeDef;
        functionType_FindNext: TTypeDef;
        procedureType_FindClose: TTypeDef;
        functionType_PChar_LongInt: TTypeDef;
        functionType_StrToIntDef: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode, SetTypeDef, EnumTypeDef, EnumMemberTypeDef, RecordTypeDef;

destructor TSysutilsUnit.Destroy;
begin
    if loaded then
    begin
        functionType_String_String.Free;
        functionType_LongInt_String.Free;
        functionType_String_LongInt.Free;
        typeReplaceFlag.Free;
        memberTypeOfReplaceFlag.Free;
        setTypeOfReplaceFlags.Free;
        functionType_StringReplace.Free;
        functionType_String.Free;
        functionType_String_Boolean.Free;
        recordType_TSearchRec.Free;
        functionType_FindFirst.Free;
        functionType_FindNext.Free;
        procedureType_FindClose.Free;
        functionType_PChar_LongInt.Free;
        functionType_StrToIntDef.Free;
    end;
    inherited Destroy;
end;

procedure TSysutilsUnit.InitTypes;
begin
    functionType_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);
    functionType_LongInt_String := CreateOneParamFunctionType('v', longintType, ansiString64Type);
    functionType_String_LongInt := CreateOneParamFunctionType('s', ansiString64Type, longintType);

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

    recordType_TSearchRec := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_TSearchRec).AddMember('Time', longintType);
    TRecordTypeDef(recordType_TSearchRec).AddMember('Size', longintType);
    TRecordTypeDef(recordType_TSearchRec).AddMember('Attr', longintType);
    TRecordTypeDef(recordType_TSearchRec).AddMember('Name', ansiString64Type);
    TRecordTypeDef(recordType_TSearchRec).AddMember('ExcludeAttr', longintType);
    TRecordTypeDef(recordType_TSearchRec).AddMember('FindHandle', pointer64Type);

    functionType_FindFirst := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, 'path', ansiString64Type),
        CreateParam(ptkValue, 'attr', longintType),
        CreateParam(ptkVar, 'f', recordType_TSearchRec)
    ]), longintType);

    functionType_FindNext := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkVar, 'f', recordType_TSearchRec)
    ]), longintType);

    procedureType_FindClose := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', recordType_TSearchRec)
    ]));
    functionType_PChar_LongInt := CreateOneParamFunctionType('p', pcharType, longintType);
    functionType_StrToIntDef := CreateTwoParamFunctionType('s', ansiString64Type, 'default', longintType, longintType);
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
        RegisterSymbolByName('ExcludeTrailingPathDelimiter', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('IncludeTrailingPathDelimiter', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('ExtractFileExt', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('ExtractFilePath', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('ExpandFileName', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('StrLen', nil, skFunction, functionType_PChar_LongInt, ctx.Cursor);
        RegisterSymbolByName('StrToInt', nil, skFunction, functionType_String_LongInt, ctx.Cursor);
        RegisterSymbolByName('StrToIntDef', nil, skFunction, functionType_StrToIntDef, ctx.Cursor);
        RegisterSymbolByName('Trim', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('TrimLeft', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('TrimRight', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('PathDelim', nil, skConstant, charType, ctx.Cursor);
        RegisterSymbolByName('DriveDelim', nil, skConstant, charType, ctx.Cursor);
        RegisterSymbolByName('PathSep', nil, skConstant, charType, ctx.Cursor);

        RegisterSymbolByName('TSearchRec', nil, skTypeName, recordType_TSearchRec, ctx.Cursor);

        RegisterSymbolByName('faReadOnly', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faHidden', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faSysFile', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faVolumeID', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faDirectory', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faArchive', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('faAnyFile', nil, skConstant, longintType, ctx.Cursor);

        RegisterSymbolByName('FindFirst', nil, skFunction, functionType_FindFirst, ctx.Cursor);
        RegisterSymbolByName('FindNext', nil, skFunction, functionType_FindNext, ctx.Cursor);
        RegisterSymbolByName('FindClose', nil, skProcedure, procedureType_FindClose, ctx.Cursor);
    end;
end;

end.
