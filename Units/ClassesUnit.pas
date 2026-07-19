unit ClassesUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TClassesUnit = class(TSystemUnit)
    private
        classType_TFPList: TTypeDef;
        classType_TStrings: TTypeDef;
        classType_TStringList: TTypeDef;
        dynArrayOfPointerType: TTypeDef;
        dynArrayOfStringType: TTypeDef;

        func_Create_TFPList: TTypeDef;
        func_Create_TStrings: TTypeDef;
        func_Create_TStringList: TTypeDef;

        func_Pointer_LongInt: TTypeDef;
        func_ItemDirection_LongInt: TTypeDef;
        func_Void_Pointer: TTypeDef;
        func_Void_TFPList: TTypeDef;
        func_Pointer_Pointer: TTypeDef;
        func_String_LongInt: TTypeDef;
        func_StringPointer_LongInt: TTypeDef;
        func_StringString_TStrings: TTypeDef;
        func_String_String: TTypeDef;
        func_Void_String: TTypeDef;
        func_Pointer_Boolean: TTypeDef;
        func_StringVarLongInt_Boolean: TTypeDef;

        proc_LongInt: TTypeDef;
        proc_LongInt_LongInt: TTypeDef;
        proc_LongInt_Pointer: TTypeDef;
        proc_LongInt_String: TTypeDef;
        proc_LongIntStringPointer: TTypeDef;
        proc_String: TTypeDef;
        proc_Pointer: TTypeDef;
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

destructor TClassesUnit.Destroy;
begin
    if loaded then
    begin
        classType_TFPList.Free;
        classType_TStrings.Free;
        classType_TStringList.Free;

        dynArrayOfPointerType.Free;
        dynArrayOfStringType.Free;

        func_Create_TFPList.Free;
        func_Create_TStrings.Free;
        func_Create_TStringList.Free;

        func_Pointer_LongInt.Free;
        func_ItemDirection_LongInt.Free;
        func_Void_Pointer.Free;
        func_Void_TFPList.Free;
        func_Pointer_Pointer.Free;

        func_String_LongInt.Free;
        func_StringPointer_LongInt.Free;
        func_StringString_TStrings.Free;
        func_String_String.Free;
        func_Void_String.Free;
        func_Pointer_Boolean.Free;
        func_StringVarLongInt_Boolean.Free;

        proc_LongInt.Free;
        proc_LongInt_LongInt.Free;
        proc_LongInt_Pointer.Free;
        proc_LongInt_String.Free;
        proc_LongIntStringPointer.Free;
        proc_String.Free;
        proc_Pointer.Free;
        proc_Pointer_Pointer.Free;
    end;
    inherited Destroy;
end;

procedure TClassesUnit.InitTypes;
begin
    dynArrayOfPointerType := TDynamicArrayTypeDef.Create(nil, pointer64Type, 8);
    dynArrayOfStringType := TDynamicArrayTypeDef.Create(nil, ansiString64Type, 8);

    // TFPList
    classType_TFPList := TClassTypeDef.Create;

    // TStrings
    classType_TStrings := TClassTypeDef.Create;

    // TStringList
    classType_TStringList := TClassTypeDef.Create;
    TClassTypeDef(classType_TStringList).parentClass := classType_TStrings;

    func_Create_TFPList := CreateFunctionType(TParameterList.Create, classType_TFPList);
    func_Create_TStrings := CreateFunctionType(TParameterList.Create, classType_TStrings);
    func_Create_TStringList := CreateFunctionType(TParameterList.Create, classType_TStringList);

    func_Pointer_LongInt := CreateOneParamFunctionType('item', pointer64Type, longintType);
    func_ItemDirection_LongInt := CreateTwoParamFunctionType('item', pointer64Type, 'direction', longintType, longintType);
    func_Void_Pointer := CreateFunctionType(TParameterList.Create, pointer64Type);
    func_Void_TFPList := CreateFunctionType(TParameterList.Create, classType_TFPList);
    func_Pointer_Pointer := CreateOneParamFunctionType('item', pointer64Type, pointer64Type);

    func_String_LongInt := CreateOneParamFunctionType('s', ansiString64Type, longintType);
    func_StringPointer_LongInt := CreateTwoParamFunctionType('s', ansiString64Type, 'aobject', pointer64Type, longintType);
    func_StringString_TStrings := CreateTwoParamFunctionType('aname', ansiString64Type, 'avalue', ansiString64Type, classType_TStrings);
    func_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);
    func_Void_String := CreateFunctionType(TParameterList.Create, ansiString64Type);
    func_Pointer_Boolean := CreateOneParamFunctionType('obj', pointer64Type, booleanType);
    func_StringVarLongInt_Boolean := CreateTwoParamVarFunctionType('s', ansiString64Type, 'index', longintType, booleanType);

    proc_LongInt := CreateOneParamProcedureType('index', longintType);
    proc_LongInt_LongInt := CreateTwoParamProcedureType('index1', longintType, 'index2', longintType);
    proc_LongInt_Pointer := CreateTwoParamProcedureType('index', longintType, 'item', pointer64Type);
    proc_LongInt_String := CreateTwoParamProcedureType('index', longintType, 's', ansiString64Type);
    proc_LongIntStringPointer := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, 'index', longintType),
        CreateParam(ptkValue, 's', ansiString64Type),
        CreateParam(ptkValue, 'aobject', pointer64Type)
    ]));
    proc_String := CreateOneParamProcedureType('s', ansiString64Type);
    proc_Pointer := CreateOneParamProcedureType('ptr', pointer64Type);
    proc_Pointer_Pointer := CreateTwoParamProcedureType('proc2call', pointer64Type, 'arg', pointer64Type);

    TClassTypeDef(classType_TFPList).classFields.Add('capacity', longintType);
    TClassTypeDef(classType_TFPList).classFields.Add('count', longintType);
    TClassTypeDef(classType_TFPList).classFields.Add('items', dynArrayOfPointerType);
    TClassTypeDef(classType_TFPList).classFields.Add('list', pointer64Type);
    TClassTypeDef(classType_TFPList).classFields.Add('create', func_Create_TFPList);
    TClassTypeDef(classType_TFPList).classFields.Add('destroy', voidProcedureType);
    TClassTypeDef(classType_TFPList).classFields.Add('free', voidProcedureType);
    TClassTypeDef(classType_TFPList).classFields.Add('add', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('addlist', proc_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('assign', proc_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('clear', voidProcedureType);
    TClassTypeDef(classType_TFPList).classFields.Add('delete', proc_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('exchange', proc_LongInt_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('expand', func_Void_TFPList);
    TClassTypeDef(classType_TFPList).classFields.Add('extract', func_Pointer_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('first', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('getenumerator', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('indexof', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('indexofitem', func_ItemDirection_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('insert', proc_LongInt_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('last', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('move', proc_LongInt_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('pack', voidProcedureType);
    TClassTypeDef(classType_TFPList).classFields.Add('remove', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).classFields.Add('sort', proc_Pointer);
    TClassTypeDef(classType_TFPList).classFields.Add('foreachcall', proc_Pointer_Pointer);

    TClassTypeDef(classType_TStrings).classFields.Add('alwaysquote', booleanType);
    TClassTypeDef(classType_TStrings).classFields.Add('capacity', longintType);
    TClassTypeDef(classType_TStrings).classFields.Add('commatext', ansiString64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('count', longintType);
    TClassTypeDef(classType_TStrings).classFields.Add('defaultencoding', pointer64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('delimitedtext', ansiString64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('delimiter', charType);
    TClassTypeDef(classType_TStrings).classFields.Add('encoding', pointer64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('linebreak', ansiString64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('missingnamevalueseparatoraction', longintType);
    TClassTypeDef(classType_TStrings).classFields.Add('names', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).classFields.Add('namevalueseparator', charType);
    TClassTypeDef(classType_TStrings).classFields.Add('objects', dynArrayOfPointerType);
    TClassTypeDef(classType_TStrings).classFields.Add('options', longintType);
    TClassTypeDef(classType_TStrings).classFields.Add('quotechar', charType);
    TClassTypeDef(classType_TStrings).classFields.Add('skiplastlinebreak', booleanType);
    TClassTypeDef(classType_TStrings).classFields.Add('strictdelimiter', booleanType);
    TClassTypeDef(classType_TStrings).classFields.Add('strings', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).classFields.Add('text', ansiString64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('textlinebreakstyle', longintType);
    TClassTypeDef(classType_TStrings).classFields.Add('trailinglinebreak', booleanType);
    TClassTypeDef(classType_TStrings).classFields.Add('uselocale', booleanType);
    TClassTypeDef(classType_TStrings).classFields.Add('valuefromindex', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).classFields.Add('values', ansiString64Type);
    TClassTypeDef(classType_TStrings).classFields.Add('writebom', booleanType);

    TClassTypeDef(classType_TStrings).classFields.Add('create', func_Create_TStrings);
    TClassTypeDef(classType_TStrings).classFields.Add('destroy', voidProcedureType);
    TClassTypeDef(classType_TStrings).classFields.Add('free', voidProcedureType);
    TClassTypeDef(classType_TStrings).classFields.Add('add', func_String_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('addobject', func_StringPointer_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('addpair', func_StringString_TStrings);
    TClassTypeDef(classType_TStrings).classFields.Add('addstrings', proc_Pointer);
    TClassTypeDef(classType_TStrings).classFields.Add('addtext', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('addcommatext', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('adddelimitedtext', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('append', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('assign', proc_Pointer);
    TClassTypeDef(classType_TStrings).classFields.Add('beginupdate', voidProcedureType);
    TClassTypeDef(classType_TStrings).classFields.Add('clear', voidProcedureType);
    TClassTypeDef(classType_TStrings).classFields.Add('delete', proc_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('endupdate', voidProcedureType);
    TClassTypeDef(classType_TStrings).classFields.Add('equals', func_Pointer_Boolean);
    TClassTypeDef(classType_TStrings).classFields.Add('exchange', proc_LongInt_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('extractname', func_String_String);
    TClassTypeDef(classType_TStrings).classFields.Add('getenumerator', func_Void_Pointer);
    TClassTypeDef(classType_TStrings).classFields.Add('gettext', func_Void_String);
    TClassTypeDef(classType_TStrings).classFields.Add('indexof', func_String_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('indexofname', func_String_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('indexofobject', func_Pointer_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('insert', proc_LongInt_String);
    TClassTypeDef(classType_TStrings).classFields.Add('insertobject', proc_LongIntStringPointer);
    TClassTypeDef(classType_TStrings).classFields.Add('lastindexof', func_String_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('loadfromfile', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('loadfromstream', proc_Pointer);
    TClassTypeDef(classType_TStrings).classFields.Add('move', proc_LongInt_LongInt);
    TClassTypeDef(classType_TStrings).classFields.Add('pop', func_Void_String);
    TClassTypeDef(classType_TStrings).classFields.Add('savetofile', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('savetostream', proc_Pointer);
    TClassTypeDef(classType_TStrings).classFields.Add('settext', proc_String);
    TClassTypeDef(classType_TStrings).classFields.Add('shift', func_Void_String);

    TClassTypeDef(classType_TStringList).classFields.Add('duplicates', longintType);
    TClassTypeDef(classType_TStringList).classFields.Add('sorted', booleanType);
    TClassTypeDef(classType_TStringList).classFields.Add('casesensitive', booleanType);
    TClassTypeDef(classType_TStringList).classFields.Add('ownsobjects', booleanType);
    TClassTypeDef(classType_TStringList).classFields.Add('sortstyle', longintType);

    TClassTypeDef(classType_TStringList).classFields.Add('create', func_Create_TStringList);
    TClassTypeDef(classType_TStringList).classFields.Add('destroy', voidProcedureType);
    TClassTypeDef(classType_TStringList).classFields.Add('free', voidProcedureType);
    TClassTypeDef(classType_TStringList).classFields.Add('find', func_StringVarLongInt_Boolean);
    TClassTypeDef(classType_TStringList).classFields.Add('sort', voidProcedureType);
    TClassTypeDef(classType_TStringList).classFields.Add('customsort', proc_Pointer);
end;

procedure TClassesUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPList', nil, skTypeName, classType_TFPList, ctx.Cursor);
        RegisterSymbolByName('TStrings', nil, skTypeName, classType_TStrings, ctx.Cursor);
        RegisterSymbolByName('TStringList', nil, skTypeName, classType_TStringList, ctx.Cursor);
    end;
end;

end.
