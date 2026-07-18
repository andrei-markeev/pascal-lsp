unit ContnrsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

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
        if classType_TFPHashList is TClassTypeDef then
            TClassTypeDef(classType_TFPHashList).classFields.Free;
    inherited Destroy;
end;

procedure TContnrsUnit.InitTypes;
begin
    dynArrayOfPointerType := TDynamicArrayTypeDef.Create(pointer64Type, 8);

    classType_TFPHashList := TClassTypeDef.Create(TFPHashList.Create, nil, 0);

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

    TClassTypeDef(classType_TFPHashList).classFields.Add('capacity', longintType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('count', longintType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('items', dynArrayOfPointerType);

    TClassTypeDef(classType_TFPHashList).classFields.Add('create', func_Create_TFPHashList);
    TClassTypeDef(classType_TFPHashList).classFields.Add('destroy', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('free', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('add', func_StringPointer_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('clear', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('delete', proc_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('expand', func_Void_TFPHashList);
    TClassTypeDef(classType_TFPHashList).classFields.Add('extract', func_Pointer_Pointer);
    TClassTypeDef(classType_TFPHashList).classFields.Add('find', func_String_Pointer);
    TClassTypeDef(classType_TFPHashList).classFields.Add('findindexof', func_String_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('findwithhash', func_StringLongWord_Pointer);
    TClassTypeDef(classType_TFPHashList).classFields.Add('getnextcollision', func_LongInt_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('hashofindex', func_LongInt_LongWord);
    TClassTypeDef(classType_TFPHashList).classFields.Add('indexof', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('nameofindex', func_LongInt_String);
    TClassTypeDef(classType_TFPHashList).classFields.Add('pack', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('remove', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('rename', func_StringString_LongInt);
    TClassTypeDef(classType_TFPHashList).classFields.Add('showstatistics', voidProcedureType);
    TClassTypeDef(classType_TFPHashList).classFields.Add('foreachcall', proc_Pointer_Pointer);
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
