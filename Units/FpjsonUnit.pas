unit FpjsonUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TFpjsonUnit = class(TSystemUnit)
    private
        enumType_TJSONtype: TTypeDef;
        classType_TJSONData: TTypeDef;
        classType_TJSONNumber: TTypeDef;
        classType_TJSONInteger: TTypeDef;
        classType_TJSONInt64: TTypeDef;
        classType_TJSONFloat: TTypeDef;
        classType_TJSONString: TTypeDef;
        classType_TJSONBoolean: TTypeDef;
        classType_TJSONNull: TTypeDef;
        classType_TJSONArray: TTypeDef;
        classType_TJSONObject: TTypeDef;
        classType_TJSONEnum: TTypeDef;

        func_Create_TJSONData: TTypeDef;
        func_Create_TJSONArray: TTypeDef;
        func_Create_TJSONObject: TTypeDef;
        func_String_TJSONData: TTypeDef;
        func_TJSONData_LongInt: TTypeDef;
        func_StringTJSONData_LongInt: TTypeDef;
        func_TJSONData_Boolean: TTypeDef;
        func_String_String: TTypeDef;

        proc_LongInt: TTypeDef;
        proc_TJSONData: TTypeDef;
        proc_LongIntTJSONData: TTypeDef;
        proc_String: TTypeDef;

    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, CompilationMode, Parameters, ClassTypeDef, EnumTypeDef, DynamicArrayTypeDef, SystemUnits;

destructor TFpjsonUnit.Destroy;
begin
    if loaded then
    begin
        enumType_TJSONtype.Free;
        classType_TJSONData.Free;
        classType_TJSONNumber.Free;
        classType_TJSONInteger.Free;
        classType_TJSONInt64.Free;
        classType_TJSONFloat.Free;
        classType_TJSONString.Free;
        classType_TJSONBoolean.Free;
        classType_TJSONNull.Free;
        classType_TJSONArray.Free;
        classType_TJSONObject.Free;
        classType_TJSONEnum.Free;

        func_Create_TJSONData.Free;
        func_Create_TJSONArray.Free;
        func_Create_TJSONObject.Free;
        func_String_TJSONData.Free;
        func_TJSONData_LongInt.Free;
        func_StringTJSONData_LongInt.Free;
        func_TJSONData_Boolean.Free;
        func_String_String.Free;

        proc_LongInt.Free;
        proc_TJSONData.Free;
        proc_LongIntTJSONData.Free;
        proc_String.Free;
    end;
    inherited Destroy;
end;

procedure TFpjsonUnit.InitTypes;
begin
    // TJSONtype enum
    enumType_TJSONtype := TEnumTypeDef.Create(nil);
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtUnknown');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtNumber');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtString');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtBoolean');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtNull');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtArray');
    TEnumTypeDef(enumType_TJSONtype).AddMember('jtObject');

    // Base JSON class
    classType_TJSONData := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONData).parentClass := classType_TObject;

    classType_TJSONNumber := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONNumber).parentClass := classType_TJSONData;

    classType_TJSONInteger := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONInteger).parentClass := classType_TJSONNumber;

    classType_TJSONInt64 := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONInt64).parentClass := classType_TJSONNumber;

    classType_TJSONFloat := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONFloat).parentClass := classType_TJSONNumber;

    classType_TJSONString := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONString).parentClass := classType_TJSONData;

    classType_TJSONBoolean := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONBoolean).parentClass := classType_TJSONData;

    classType_TJSONNull := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONNull).parentClass := classType_TJSONData;

    classType_TJSONArray := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONArray).parentClass := classType_TJSONData;

    classType_TJSONObject := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONObject).parentClass := classType_TJSONData;

    classType_TJSONEnum := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONEnum).parentClass := classType_TObject;

    func_Create_TJSONData := CreateFunctionType(TParameterList.Create, classType_TJSONData);
    func_Create_TJSONArray := CreateFunctionType(TParameterList.Create, classType_TJSONArray);
    func_Create_TJSONObject := CreateFunctionType(TParameterList.Create, classType_TJSONObject);

    func_String_TJSONData := CreateOneParamFunctionType('path', ansiString64Type, classType_TJSONData);
    func_TJSONData_LongInt := CreateOneParamFunctionType('item', classType_TJSONData, longintType);
    func_StringTJSONData_LongInt := CreateTwoParamFunctionType('aname', ansiString64Type, 'value', classType_TJSONData, longintType);
    func_TJSONData_Boolean := CreateOneParamFunctionType('item', classType_TJSONData, booleanType);
    func_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);

    proc_LongInt := CreateOneParamProcedureType('index', longintType);
    proc_TJSONData := CreateOneParamProcedureType('item', classType_TJSONData);
    proc_LongIntTJSONData := CreateTwoParamProcedureType('index', longintType, 'item', classType_TJSONData);
    proc_String := CreateOneParamProcedureType('s', ansiString64Type);

    // TJSONData members
    TClassTypeDef(classType_TJSONData).AddMember('JSONType', enumType_TJSONtype);
    TClassTypeDef(classType_TJSONData).AddMember('AsJSON', ansiString64Type);
    TClassTypeDef(classType_TJSONData).AddMember('AsString', ansiString64Type);
    TClassTypeDef(classType_TJSONData).AddMember('AsInteger', longintType);
    TClassTypeDef(classType_TJSONData).AddMember('AsInt64', int64Type);
    TClassTypeDef(classType_TJSONData).AddMember('AsFloat', doubleType);
    TClassTypeDef(classType_TJSONData).AddMember('AsBoolean', booleanType);
    TClassTypeDef(classType_TJSONData).AddMember('IsNull', booleanType);
    TClassTypeDef(classType_TJSONData).AddMember('Count', longintType);
    TClassTypeDef(classType_TJSONData).AddMember('Value', unknownType);
    TClassTypeDef(classType_TJSONData).AddMember('Items', classType_TJSONData);

    TClassTypeDef(classType_TJSONData).AddMember('Create', func_Create_TJSONData);
    TClassTypeDef(classType_TJSONData).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TJSONData).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TJSONData).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TJSONData).AddMember('Clone', func_Create_TJSONData);
    TClassTypeDef(classType_TJSONData).AddMember('FindPath', func_String_TJSONData);
    TClassTypeDef(classType_TJSONData).AddMember('GetPath', func_String_TJSONData);
    TClassTypeDef(classType_TJSONData).AddMember('FormatJSON', func_Create_TJSONData);

    // TJSONArray members
    TClassTypeDef(classType_TJSONArray).AddMember('Create', func_Create_TJSONArray);
    TClassTypeDef(classType_TJSONArray).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TJSONArray).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TJSONArray).AddMember('Add', func_TJSONData_LongInt);
    TClassTypeDef(classType_TJSONArray).AddMember('Delete', proc_LongInt);
    TClassTypeDef(classType_TJSONArray).AddMember('Remove', proc_TJSONData);
    TClassTypeDef(classType_TJSONArray).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TJSONArray).AddMember('IndexOf', func_TJSONData_LongInt);
    TClassTypeDef(classType_TJSONArray).AddMember('Insert', proc_LongIntTJSONData);

    // TJSONObject members
    TClassTypeDef(classType_TJSONObject).AddMember('Create', func_Create_TJSONObject);
    TClassTypeDef(classType_TJSONObject).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TJSONObject).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TJSONObject).AddMember('Add', func_StringTJSONData_LongInt);
    TClassTypeDef(classType_TJSONObject).AddMember('Delete', proc_LongInt);
    TClassTypeDef(classType_TJSONObject).AddMember('Remove', proc_TJSONData);
    TClassTypeDef(classType_TJSONObject).AddMember('Extract', func_String_TJSONData);
    TClassTypeDef(classType_TJSONObject).AddMember('IndexOfName', func_String_TJSONData);
    TClassTypeDef(classType_TJSONObject).AddMember('IndexOfObject', func_TJSONData_LongInt);
    TClassTypeDef(classType_TJSONObject).AddMember('Find', func_String_TJSONData);
    TClassTypeDef(classType_TJSONObject).AddMember('Get', func_String_TJSONData);
end;

procedure TFpjsonUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TJSONtype', nil, skTypeName, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtUnknown', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtNumber', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtString', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtBoolean', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtNull', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtArray', nil, skConstant, enumType_TJSONtype, ctx.Cursor);
        RegisterSymbolByName('jtObject', nil, skConstant, enumType_TJSONtype, ctx.Cursor);

        RegisterSymbolByName('TJSONData', nil, skTypeName, classType_TJSONData, ctx.Cursor);
        RegisterSymbolByName('TJSONNumber', nil, skTypeName, classType_TJSONNumber, ctx.Cursor);
        RegisterSymbolByName('TJSONInteger', nil, skTypeName, classType_TJSONInteger, ctx.Cursor);
        RegisterSymbolByName('TJSONInt64', nil, skTypeName, classType_TJSONInt64, ctx.Cursor);
        RegisterSymbolByName('TJSONFloat', nil, skTypeName, classType_TJSONFloat, ctx.Cursor);
        RegisterSymbolByName('TJSONString', nil, skTypeName, classType_TJSONString, ctx.Cursor);
        RegisterSymbolByName('TJSONBoolean', nil, skTypeName, classType_TJSONBoolean, ctx.Cursor);
        RegisterSymbolByName('TJSONNull', nil, skTypeName, classType_TJSONNull, ctx.Cursor);
        RegisterSymbolByName('TJSONArray', nil, skTypeName, classType_TJSONArray, ctx.Cursor);
        RegisterSymbolByName('TJSONObject', nil, skTypeName, classType_TJSONObject, ctx.Cursor);
        RegisterSymbolByName('TJSONEnum', nil, skTypeName, classType_TJSONEnum, ctx.Cursor);
        RegisterSymbolByName('StringToJSONString', nil, skFunction, func_String_String, ctx.Cursor);
    end;
end;

end.
