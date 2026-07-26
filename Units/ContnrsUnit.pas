unit ContnrsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TContnrsUnit = class(TSystemUnit)
    private
        classType_TFPHashList: TTypeDef;
        dynArrayOfPointerType: TTypeDef;

        func_Create_TFPHashList: TTypeDef;
        func_StringPointer_LongInt: TTypeDef;
        func_Void_TFPHashList: TTypeDef;
        func_Pointer_Pointer: TTypeDef;
        func_String_Pointer: TTypeDef;
        func_String_LongInt: TTypeDef;
        func_StringLongWord_Pointer: TTypeDef;
        func_LongInt_LongInt: TTypeDef;
        func_LongInt_LongWord: TTypeDef;
        func_Pointer_LongInt: TTypeDef;
        func_LongInt_String: TTypeDef;
        func_StringString_LongInt: TTypeDef;

        proc_LongInt: TTypeDef;
        proc_Pointer_Pointer: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    contnrs, Symbols, CompilationMode, Parameters, ClassTypeDef, DynamicArrayTypeDef;

destructor TContnrsUnit.Destroy;
begin
    if loaded then
    begin
        classType_TFPHashList.Free;
        dynArrayOfPointerType.Free;

        func_Create_TFPHashList.Free;
        func_StringPointer_LongInt.Free;
        func_Void_TFPHashList.Free;
        func_Pointer_Pointer.Free;
        func_String_Pointer.Free;
        func_String_LongInt.Free;
        func_StringLongWord_Pointer.Free;
        func_LongInt_LongInt.Free;
        func_LongInt_LongWord.Free;
        func_Pointer_LongInt.Free;
        func_LongInt_String.Free;
        func_StringString_LongInt.Free;

        proc_LongInt.Free;
        proc_Pointer_Pointer.Free;
    end;
    inherited Destroy;
end;

procedure TContnrsUnit.InitTypes;
begin
    dynArrayOfPointerType := TDynamicArrayTypeDef.Create(nil, pointer64Type, 8);

    classType_TFPHashList := TClassTypeDef.Create;

    func_Create_TFPHashList := CreateFunctionType(TParameterList.Create, classType_TFPHashList);
    func_StringPointer_LongInt := CreateTwoParamFunctionType('aname', ansiString64Type, 'item', pointer64Type, longintType);
    func_Void_TFPHashList := CreateFunctionType(TParameterList.Create, classType_TFPHashList);
    func_Pointer_Pointer := CreateOneParamFunctionType('item', pointer64Type, pointer64Type);
    func_String_Pointer := CreateOneParamFunctionType('aname', ansiString64Type, pointer64Type);
    func_String_LongInt := CreateOneParamFunctionType('aname', ansiString64Type, longintType);
    func_StringLongWord_Pointer := CreateTwoParamFunctionType('aname', ansiString64Type, 'ahash', longwordType, pointer64Type);
    func_LongInt_LongInt := CreateOneParamFunctionType('index', longintType, longintType);
    func_LongInt_LongWord := CreateOneParamFunctionType('index', longintType, longwordType);
    func_Pointer_LongInt := CreateOneParamFunctionType('item', pointer64Type, longintType);
    func_LongInt_String := CreateOneParamFunctionType('index', longintType, ansiString64Type);
    func_StringString_LongInt := CreateTwoParamFunctionType('aoldname', ansiString64Type, 'anewname', ansiString64Type, longintType);

    proc_LongInt := CreateOneParamProcedureType('index', longintType);
    proc_Pointer_Pointer := CreateTwoParamProcedureType('proc2call', pointer64Type, 'arg', pointer64Type);

    TClassTypeDef(classType_TFPHashList).AddMember('Capacity', longintType);
    TClassTypeDef(classType_TFPHashList).AddMember('Count', longintType);
    TClassTypeDef(classType_TFPHashList).AddMember('Items', dynArrayOfPointerType);

    TClassTypeDef(classType_TFPHashList).AddMember('Create', func_Create_TFPHashList);
    TClassTypeDef(classType_TFPHashList).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).AddMember('Add', func_StringPointer_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).AddMember('Delete', proc_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('Expand', func_Void_TFPHashList);
    TClassTypeDef(classType_TFPHashList).AddMember('Extract', func_Pointer_Pointer);
    TClassTypeDef(classType_TFPHashList).AddMember('Find', func_String_Pointer);
    TClassTypeDef(classType_TFPHashList).AddMember('FindIndexOf', func_String_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('FindWithHash', func_StringLongWord_Pointer);
    TClassTypeDef(classType_TFPHashList).AddMember('GetNextCollision', func_LongInt_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('HashOfIndex', func_LongInt_LongWord);
    TClassTypeDef(classType_TFPHashList).AddMember('IndexOf', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('NameOfIndex', func_LongInt_String);
    TClassTypeDef(classType_TFPHashList).AddMember('Pack', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).AddMember('Remove', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('Rename', func_StringString_LongInt);
    TClassTypeDef(classType_TFPHashList).AddMember('ShowStatistics', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).AddMember('ForEachCall', proc_Pointer_Pointer);
end;

procedure TContnrsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPHashList', nil, skTypeName, classType_TFPHashList, ctx.Cursor);
    end;
end;

end.
