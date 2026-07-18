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
    contnrs, Symbols, CompilationMode, Parameters;

destructor TContnrsUnit.Destroy;
begin
    if loaded then
        classType_TFPHashList.classFields.Free;
    inherited Destroy;
end;

procedure TContnrsUnit.InitTypes;
begin
    dynArrayOfPointerType.kind := tkDynamicArray;
    dynArrayOfPointerType.size := 8;
    dynArrayOfPointerType.typeOfDynValues := @pointer64Type;

    classType_TFPHashList.kind := tkClass;
    classType_TFPHashList.size := 0;
    classType_TFPHashList.classFields := TFPHashList.Create;
    classType_TFPHashList.parentClass := nil;

    func_Create_TFPHashList := CreateFunctionType(TParameterList.Create, @classType_TFPHashList);
    func_StringPointer_LongInt := CreateTwoParamFunctionType('aname', @ansiString64Type, 'item', @pointer64Type, @longintType);
    func_Void_TFPHashList := CreateFunctionType(TParameterList.Create, @classType_TFPHashList);
    func_Pointer_Pointer := CreateOneParamFunctionType('item', @pointer64Type, @pointer64Type);
    func_String_Pointer := CreateOneParamFunctionType('aname', @ansiString64Type, @pointer64Type);
    func_String_LongInt := CreateOneParamFunctionType('aname', @ansiString64Type, @longintType);
    func_StringLongWord_Pointer := CreateTwoParamFunctionType('aname', @ansiString64Type, 'ahash', @longwordType, @pointer64Type);
    func_LongInt_LongInt := CreateOneParamFunctionType('index', @longintType, @longintType);
    func_LongInt_LongWord := CreateOneParamFunctionType('index', @longintType, @longwordType);
    func_Pointer_LongInt := CreateOneParamFunctionType('item', @pointer64Type, @longintType);
    func_LongInt_String := CreateOneParamFunctionType('index', @longintType, @ansiString64Type);
    func_StringString_LongInt := CreateTwoParamFunctionType('aoldname', @ansiString64Type, 'anewname', @ansiString64Type, @longintType);

    proc_LongInt := CreateOneParamProcedureType('index', @longintType);
    proc_Pointer_Pointer := CreateTwoParamProcedureType('proc2call', @pointer64Type, 'arg', @pointer64Type);

    classType_TFPHashList.classFields.Add('capacity', @longintType);
    classType_TFPHashList.classFields.Add('count', @longintType);
    classType_TFPHashList.classFields.Add('items', @dynArrayOfPointerType);

    classType_TFPHashList.classFields.Add('create', @func_Create_TFPHashList);
    classType_TFPHashList.classFields.Add('destroy', @voidProcedureType);
    classType_TFPHashList.classFields.Add('free', @voidProcedureType);
    classType_TFPHashList.classFields.Add('add', @func_StringPointer_LongInt);
    classType_TFPHashList.classFields.Add('clear', @voidProcedureType);
    classType_TFPHashList.classFields.Add('delete', @proc_LongInt);
    classType_TFPHashList.classFields.Add('expand', @func_Void_TFPHashList);
    classType_TFPHashList.classFields.Add('extract', @func_Pointer_Pointer);
    classType_TFPHashList.classFields.Add('find', @func_String_Pointer);
    classType_TFPHashList.classFields.Add('findindexof', @func_String_LongInt);
    classType_TFPHashList.classFields.Add('findwithhash', @func_StringLongWord_Pointer);
    classType_TFPHashList.classFields.Add('getnextcollision', @func_LongInt_LongInt);
    classType_TFPHashList.classFields.Add('hashofindex', @func_LongInt_LongWord);
    classType_TFPHashList.classFields.Add('indexof', @func_Pointer_LongInt);
    classType_TFPHashList.classFields.Add('nameofindex', @func_LongInt_String);
    classType_TFPHashList.classFields.Add('pack', @voidProcedureType);
    classType_TFPHashList.classFields.Add('remove', @func_Pointer_LongInt);
    classType_TFPHashList.classFields.Add('rename', @func_StringString_LongInt);
    classType_TFPHashList.classFields.Add('showstatistics', @voidProcedureType);
    classType_TFPHashList.classFields.Add('foreachcall', @proc_Pointer_Pointer);
end;

procedure TContnrsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPHashList', nil, skTypeName, @classType_TFPHashList, ctx.Cursor);
    end;
end;

end.
