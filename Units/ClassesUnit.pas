unit ClassesUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TClassesUnit = class(TSystemUnit)
    private
        classType_TFPList: TTypeDef;
        classType_TStrings: TTypeDef;
        classType_TStringList: TTypeDef;
        classType_TStream: TTypeDef;
        dynArrayOfPointerType: TTypeDef;
        dynArrayOfStringType: TTypeDef;

        func_Create_TFPList: TTypeDef;
        func_Create_TStrings: TTypeDef;
        func_Create_TStringList: TTypeDef;
        func_Create_TStream: TTypeDef;

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
        func_BufferLongInt_LongInt: TTypeDef;
        func_LongIntWord_LongInt: TTypeDef;
        func_TStreamInt64_Int64: TTypeDef;

        proc_LongInt: TTypeDef;
        proc_LongInt_LongInt: TTypeDef;
        proc_LongInt_Pointer: TTypeDef;
        proc_LongInt_String: TTypeDef;
        proc_LongIntStringPointer: TTypeDef;
        proc_String: TTypeDef;
        proc_Pointer: TTypeDef;
        proc_Pointer_Pointer: TTypeDef;
        proc_BufferLongInt: TTypeDef;
        proc_TStream: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    contnrs, Symbols, CompilationMode, Parameters, ClassTypeDef, DynamicArrayTypeDef, SystemUnits;

destructor TClassesUnit.Destroy;
begin
    if loaded then
    begin
        classType_TFPList.Free;
        classType_TStrings.Free;
        classType_TStringList.Free;
        classType_TStream.Free;

        dynArrayOfPointerType.Free;
        dynArrayOfStringType.Free;

        func_Create_TFPList.Free;
        func_Create_TStrings.Free;
        func_Create_TStringList.Free;
        func_Create_TStream.Free;

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
        func_BufferLongInt_LongInt.Free;
        func_LongIntWord_LongInt.Free;
        func_TStreamInt64_Int64.Free;

        proc_LongInt.Free;
        proc_LongInt_LongInt.Free;
        proc_LongInt_Pointer.Free;
        proc_LongInt_String.Free;
        proc_LongIntStringPointer.Free;
        proc_String.Free;
        proc_Pointer.Free;
        proc_Pointer_Pointer.Free;
        proc_BufferLongInt.Free;
        proc_TStream.Free;
    end;
    inherited Destroy;
end;

procedure TClassesUnit.InitTypes;
begin
    dynArrayOfPointerType := TDynamicArrayTypeDef.Create(nil, pointer64Type, 8);
    dynArrayOfStringType := TDynamicArrayTypeDef.Create(nil, ansiString64Type, 8);

    // TFPList
    classType_TFPList := TClassTypeDef.Create;
    TClassTypeDef(classType_TFPList).parentClass := classType_TObject;

    // TStrings
    classType_TStrings := TClassTypeDef.Create;
    TClassTypeDef(classType_TStrings).parentClass := classType_TObject;

    // TStringList
    classType_TStringList := TClassTypeDef.Create;
    TClassTypeDef(classType_TStringList).parentClass := classType_TStrings;

    // TStream
    classType_TStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TStream).parentClass := classType_TObject;

    func_Create_TFPList := CreateFunctionType(TParameterList.Create, classType_TFPList);
    func_Create_TStrings := CreateFunctionType(TParameterList.Create, classType_TStrings);
    func_Create_TStringList := CreateFunctionType(TParameterList.Create, classType_TStringList);
    func_Create_TStream := CreateFunctionType(TParameterList.Create, classType_TStream);

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
    func_BufferLongInt_LongInt := CreateTwoParamVarFunctionType('buffer', unknownType, 'count', longintType, longintType);
    func_LongIntWord_LongInt := CreateTwoParamFunctionType('offset', longintType, 'origin', wordType, longintType);
    func_TStreamInt64_Int64 := CreateTwoParamFunctionType('source', classType_TStream, 'count', int64Type, int64Type);

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
    proc_BufferLongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'buffer', unknownType),
        CreateParam(ptkValue, 'count', longintType)
    ]));
    proc_TStream := CreateOneParamProcedureType('stream', classType_TStream);

    TClassTypeDef(classType_TFPList).AddMember('Capacity', longintType);
    TClassTypeDef(classType_TFPList).AddMember('Count', longintType);
    TClassTypeDef(classType_TFPList).AddMember('Items', dynArrayOfPointerType);
    TClassTypeDef(classType_TFPList).AddMember('List', pointer64Type);
    TClassTypeDef(classType_TFPList).AddMember('Create', func_Create_TFPList);
    TClassTypeDef(classType_TFPList).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TFPList).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TFPList).AddMember('Add', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('AddList', proc_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('Assign', proc_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TFPList).AddMember('Delete', proc_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('Exchange', proc_LongInt_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('Expand', func_Void_TFPList);
    TClassTypeDef(classType_TFPList).AddMember('Extract', func_Pointer_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('First', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('GetEnumerator', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('IndexOf', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('IndexOfItem', func_ItemDirection_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('Insert', proc_LongInt_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('Last', func_Void_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('Move', proc_LongInt_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('Pack', voidProcedureType);
    TClassTypeDef(classType_TFPList).AddMember('Remove', func_Pointer_LongInt);
    TClassTypeDef(classType_TFPList).AddMember('Sort', proc_Pointer);
    TClassTypeDef(classType_TFPList).AddMember('ForEachCall', proc_Pointer_Pointer);

    TClassTypeDef(classType_TStrings).AddMember('AlwaysQuote', booleanType);
    TClassTypeDef(classType_TStrings).AddMember('Capacity', longintType);
    TClassTypeDef(classType_TStrings).AddMember('CommaText', ansiString64Type);
    TClassTypeDef(classType_TStrings).AddMember('Count', longintType);
    TClassTypeDef(classType_TStrings).AddMember('DefaultEncoding', pointer64Type);
    TClassTypeDef(classType_TStrings).AddMember('DelimitedText', ansiString64Type);
    TClassTypeDef(classType_TStrings).AddMember('Delimiter', charType);
    TClassTypeDef(classType_TStrings).AddMember('Encoding', pointer64Type);
    TClassTypeDef(classType_TStrings).AddMember('LineBreak', ansiString64Type);
    TClassTypeDef(classType_TStrings).AddMember('MissingNameValueSeparatorAction', longintType);
    TClassTypeDef(classType_TStrings).AddMember('Names', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).AddMember('NameValueSeparator', charType);
    TClassTypeDef(classType_TStrings).AddMember('Objects', dynArrayOfPointerType);
    TClassTypeDef(classType_TStrings).AddMember('Options', longintType);
    TClassTypeDef(classType_TStrings).AddMember('QuoteChar', charType);
    TClassTypeDef(classType_TStrings).AddMember('SkipLastLineBreak', booleanType);
    TClassTypeDef(classType_TStrings).AddMember('StrictDelimiter', booleanType);
    TClassTypeDef(classType_TStrings).AddMember('Strings', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).AddMember('Text', ansiString64Type);
    TClassTypeDef(classType_TStrings).AddMember('TextLineBreakStyle', longintType);
    TClassTypeDef(classType_TStrings).AddMember('TrailingLineBreak', booleanType);
    TClassTypeDef(classType_TStrings).AddMember('UseLocale', booleanType);
    TClassTypeDef(classType_TStrings).AddMember('ValueFromIndex', dynArrayOfStringType);
    TClassTypeDef(classType_TStrings).AddMember('Values', ansiString64Type);
    TClassTypeDef(classType_TStrings).AddMember('WriteBOM', booleanType);

    TClassTypeDef(classType_TStrings).AddMember('Create', func_Create_TStrings);
    TClassTypeDef(classType_TStrings).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TStrings).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TStrings).AddMember('Add', func_String_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('AddObject', func_StringPointer_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('AddPair', func_StringString_TStrings);
    TClassTypeDef(classType_TStrings).AddMember('AddStrings', proc_Pointer);
    TClassTypeDef(classType_TStrings).AddMember('AddText', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('AddCommaText', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('AddDelimitedText', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('Append', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('Assign', proc_Pointer);
    TClassTypeDef(classType_TStrings).AddMember('BeginUpdate', voidProcedureType);
    TClassTypeDef(classType_TStrings).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TStrings).AddMember('Delete', proc_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('EndUpdate', voidProcedureType);
    TClassTypeDef(classType_TStrings).AddMember('Equals', func_Pointer_Boolean);
    TClassTypeDef(classType_TStrings).AddMember('Exchange', proc_LongInt_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('ExtractName', func_String_String);
    TClassTypeDef(classType_TStrings).AddMember('GetEnumerator', func_Void_Pointer);
    TClassTypeDef(classType_TStrings).AddMember('GetText', func_Void_String);
    TClassTypeDef(classType_TStrings).AddMember('IndexOf', func_String_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('IndexOfName', func_String_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('IndexOfObject', func_Pointer_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('Insert', proc_LongInt_String);
    TClassTypeDef(classType_TStrings).AddMember('InsertObject', proc_LongIntStringPointer);
    TClassTypeDef(classType_TStrings).AddMember('LastIndexOf', func_String_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('LoadFromFile', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('LoadFromStream', proc_TStream);
    TClassTypeDef(classType_TStrings).AddMember('Move', proc_LongInt_LongInt);
    TClassTypeDef(classType_TStrings).AddMember('Pop', func_Void_String);
    TClassTypeDef(classType_TStrings).AddMember('SaveToFile', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('SaveToStream', proc_TStream);
    TClassTypeDef(classType_TStrings).AddMember('SetText', proc_String);
    TClassTypeDef(classType_TStrings).AddMember('Shift', func_Void_String);

    TClassTypeDef(classType_TStringList).AddMember('Duplicates', longintType);
    TClassTypeDef(classType_TStringList).AddMember('Sorted', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('CaseSensitive', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('OwnsObjects', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('SortStyle', longintType);

    TClassTypeDef(classType_TStringList).AddMember('Create', func_Create_TStringList);
    TClassTypeDef(classType_TStringList).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TStringList).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TStringList).AddMember('Find', func_StringVarLongInt_Boolean);
    TClassTypeDef(classType_TStringList).AddMember('Sort', voidProcedureType);
    TClassTypeDef(classType_TStringList).AddMember('CustomSort', proc_Pointer);

    TClassTypeDef(classType_TStream).AddMember('Position', int64Type);
    TClassTypeDef(classType_TStream).AddMember('Size', int64Type);
    TClassTypeDef(classType_TStream).AddMember('Create', func_Create_TStream);
    TClassTypeDef(classType_TStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TStream).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TStream).AddMember('Read', func_BufferLongInt_LongInt);
    TClassTypeDef(classType_TStream).AddMember('Write', func_BufferLongInt_LongInt);
    TClassTypeDef(classType_TStream).AddMember('Seek', func_LongIntWord_LongInt);
    TClassTypeDef(classType_TStream).AddMember('ReadBuffer', proc_BufferLongInt);
    TClassTypeDef(classType_TStream).AddMember('WriteBuffer', proc_BufferLongInt);
    TClassTypeDef(classType_TStream).AddMember('CopyFrom', func_TStreamInt64_Int64);
end;

procedure TClassesUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPList', nil, skTypeName, classType_TFPList, ctx.Cursor);
        RegisterSymbolByName('TStrings', nil, skTypeName, classType_TStrings, ctx.Cursor);
        RegisterSymbolByName('TStringList', nil, skTypeName, classType_TStringList, ctx.Cursor);
        RegisterSymbolByName('TStream', nil, skTypeName, classType_TStream, ctx.Cursor);
    end;
end;

end.
