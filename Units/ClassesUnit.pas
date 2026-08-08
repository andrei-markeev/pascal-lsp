unit ClassesUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TClassesUnit = class(TSystemUnit)
    private
        classType_TPersistent: TTypeDef;
        classType_TComponent: TTypeDef;
        classType_TFPList: TTypeDef;
        classType_TStrings: TTypeDef;
        classType_TStringList: TTypeDef;
        classType_TCustomMemoryStream: TTypeDef;
        classType_TMemoryStream: TTypeDef;
        classType_TStringStream: TTypeDef;

        enumType_TSeekOrigin: TTypeDef;
        memberTypeOfSeekOrigin: TTypeDef;
        enumType_TAlignment: TTypeDef;
        memberTypeOfAlignment: TTypeDef;
        enumType_TListNotification: TTypeDef;
        memberTypeOfListNotification: TTypeDef;
        enumType_TListAssignOp: TTypeDef;
        memberTypeOfListAssignOp: TTypeDef;
        enumType_TOperation: TTypeDef;
        memberTypeOfOperation: TTypeDef;
        enumType_THelpType: TTypeDef;
        memberTypeOfHelpType: TTypeDef;
        enumType_TStreamSeekOrigin: TTypeDef;
        memberTypeOfStreamSeekOrigin: TTypeDef;
        enumType_TStringsSortStyle: TTypeDef;
        memberTypeOfStringsSortStyle: TTypeDef;
        enumType_TStringListSortStyle: TTypeDef;
        memberTypeOfStringListSortStyle: TTypeDef;

        dynArrayOfPointerType: TTypeDef;
        dynArrayOfStringType: TTypeDef;

        func_Create_TComponent: TTypeDef;
        func_Create_TFPList: TTypeDef;
        func_Create_TStrings: TTypeDef;
        func_Create_TStringList: TTypeDef;
        func_Create_TStream: TTypeDef;
        func_Create_THandleStream: TTypeDef;
        func_Create_TFileStream: TTypeDef;
        func_Create_TMemoryStream: TTypeDef;
        func_Create_TStringStream: TTypeDef;
        func_FindComponent: TTypeDef;
        proc_Assign: TTypeDef;

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
        classType_TStream: TTypeDef;
        classType_THandleStream: TTypeDef;
        classType_TFileStream: TTypeDef;
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    contnrs, Symbols, CompilationMode, Parameters, ClassTypeDef, DynamicArrayTypeDef, EnumTypeDef, EnumMemberTypeDef, SystemUnits;

destructor TClassesUnit.Destroy;
begin
    if loaded then
    begin
        classType_TPersistent.Free;
        classType_TComponent.Free;
        classType_TFPList.Free;
        classType_TStrings.Free;
        classType_TStringList.Free;
        classType_TStream.Free;
        classType_THandleStream.Free;
        classType_TFileStream.Free;
        classType_TCustomMemoryStream.Free;
        classType_TMemoryStream.Free;
        classType_TStringStream.Free;

        enumType_TSeekOrigin.Free;
        memberTypeOfSeekOrigin.Free;
        enumType_TAlignment.Free;
        memberTypeOfAlignment.Free;
        enumType_TListNotification.Free;
        memberTypeOfListNotification.Free;
        enumType_TListAssignOp.Free;
        memberTypeOfListAssignOp.Free;
        enumType_TOperation.Free;
        memberTypeOfOperation.Free;
        enumType_THelpType.Free;
        memberTypeOfHelpType.Free;
        enumType_TStreamSeekOrigin.Free;
        memberTypeOfStreamSeekOrigin.Free;
        enumType_TStringsSortStyle.Free;
        memberTypeOfStringsSortStyle.Free;
        enumType_TStringListSortStyle.Free;
        memberTypeOfStringListSortStyle.Free;

        dynArrayOfPointerType.Free;
        dynArrayOfStringType.Free;

        func_Create_TComponent.Free;
        func_Create_TFPList.Free;
        func_Create_TStrings.Free;
        func_Create_TStringList.Free;
        func_Create_TStream.Free;
        func_Create_THandleStream.Free;
        func_Create_TFileStream.Free;
        func_Create_TMemoryStream.Free;
        func_Create_TStringStream.Free;
        func_FindComponent.Free;
        proc_Assign.Free;

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

    // Enums
    enumType_TSeekOrigin := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TSeekOrigin).AddMember('soBeginning');
    TEnumTypeDef(enumType_TSeekOrigin).AddMember('soCurrent');
    TEnumTypeDef(enumType_TSeekOrigin).AddMember('soEnd');
    memberTypeOfSeekOrigin := TEnumMemberTypeDef.Create(nil, enumType_TSeekOrigin, nil);

    enumType_TAlignment := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TAlignment).AddMember('taLeftJustify');
    TEnumTypeDef(enumType_TAlignment).AddMember('taRightJustify');
    TEnumTypeDef(enumType_TAlignment).AddMember('taCenter');
    memberTypeOfAlignment := TEnumMemberTypeDef.Create(nil, enumType_TAlignment, nil);

    enumType_TListNotification := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TListNotification).AddMember('lnAdded');
    TEnumTypeDef(enumType_TListNotification).AddMember('lnExtracted');
    TEnumTypeDef(enumType_TListNotification).AddMember('lnDeleted');
    memberTypeOfListNotification := TEnumMemberTypeDef.Create(nil, enumType_TListNotification, nil);

    enumType_TListAssignOp := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laCopy');
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laAnd');
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laOr');
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laXor');
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laSrcUnique');
    TEnumTypeDef(enumType_TListAssignOp).AddMember('laDstUnique');
    memberTypeOfListAssignOp := TEnumMemberTypeDef.Create(nil, enumType_TListAssignOp, nil);

    enumType_TOperation := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TOperation).AddMember('opInsert');
    TEnumTypeDef(enumType_TOperation).AddMember('opRemove');
    memberTypeOfOperation := TEnumMemberTypeDef.Create(nil, enumType_TOperation, nil);

    enumType_THelpType := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_THelpType).AddMember('htKeyword');
    TEnumTypeDef(enumType_THelpType).AddMember('htContext');
    memberTypeOfHelpType := TEnumMemberTypeDef.Create(nil, enumType_THelpType, nil);

    enumType_TStreamSeekOrigin := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TStreamSeekOrigin).AddMember('soFromBeginning');
    TEnumTypeDef(enumType_TStreamSeekOrigin).AddMember('soFromCurrent');
    TEnumTypeDef(enumType_TStreamSeekOrigin).AddMember('soFromEnd');
    memberTypeOfStreamSeekOrigin := TEnumMemberTypeDef.Create(nil, enumType_TStreamSeekOrigin, nil);

    enumType_TStringsSortStyle := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TStringsSortStyle).AddMember('sssNone');
    TEnumTypeDef(enumType_TStringsSortStyle).AddMember('sssUnSorted');
    TEnumTypeDef(enumType_TStringsSortStyle).AddMember('sssSorted');
    memberTypeOfStringsSortStyle := TEnumMemberTypeDef.Create(nil, enumType_TStringsSortStyle, nil);

    enumType_TStringListSortStyle := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TStringListSortStyle).AddMember('sslNone');
    TEnumTypeDef(enumType_TStringListSortStyle).AddMember('sslAuto');
    TEnumTypeDef(enumType_TStringListSortStyle).AddMember('sslUser');
    memberTypeOfStringListSortStyle := TEnumMemberTypeDef.Create(nil, enumType_TStringListSortStyle, nil);

    // TPersistent
    classType_TPersistent := TClassTypeDef.Create;
    TClassTypeDef(classType_TPersistent).parentClass := classType_TObject;
    proc_Assign := CreateOneParamProcedureType('source', classType_TPersistent);
    TClassTypeDef(classType_TPersistent).AddMember('Assign', proc_Assign);
    TClassTypeDef(classType_TPersistent).AddMember('GetNamePath', func_Void_String);

    // TComponent
    classType_TComponent := TClassTypeDef.Create;
    TClassTypeDef(classType_TComponent).parentClass := classType_TPersistent;
    func_Create_TComponent := CreateOneParamFunctionType('aowner', classType_TComponent, classType_TComponent);
    func_FindComponent := CreateOneParamFunctionType('aname', ansiString64Type, classType_TComponent);
    TClassTypeDef(classType_TComponent).AddMember('Owner', classType_TComponent);
    TClassTypeDef(classType_TComponent).AddMember('Name', ansiString64Type);
    TClassTypeDef(classType_TComponent).AddMember('Tag', longintType);
    TClassTypeDef(classType_TComponent).AddMember('ComponentCount', longintType);
    TClassTypeDef(classType_TComponent).AddMember('Components', dynArrayOfPointerType);
    TClassTypeDef(classType_TComponent).AddMember('Create', func_Create_TComponent);
    TClassTypeDef(classType_TComponent).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TComponent).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TComponent).AddMember('FindComponent', func_FindComponent);

    // TFPList
    classType_TFPList := TClassTypeDef.Create;
    TClassTypeDef(classType_TFPList).parentClass := classType_TObject;

    // TStrings
    classType_TStrings := TClassTypeDef.Create;
    TClassTypeDef(classType_TStrings).parentClass := classType_TPersistent;

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

    TClassTypeDef(classType_TStringList).AddMember('Duplicates', typesMock.enumType_TDuplicates);
    TClassTypeDef(classType_TStringList).AddMember('Sorted', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('CaseSensitive', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('OwnsObjects', booleanType);
    TClassTypeDef(classType_TStringList).AddMember('SortStyle', enumType_TStringsSortStyle);

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

    classType_THandleStream := TClassTypeDef.Create;
    TClassTypeDef(classType_THandleStream).parentClass := classType_TStream;
    func_Create_THandleStream := CreateOneParamFunctionType('ahandle', longintType, classType_THandleStream);
    TClassTypeDef(classType_THandleStream).AddMember('Handle', longintType);
    TClassTypeDef(classType_THandleStream).AddMember('Create', func_Create_THandleStream);
    TClassTypeDef(classType_THandleStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_THandleStream).AddMember('Free', voidProcedureType);

    // TFileStream
    classType_TFileStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TFileStream).parentClass := classType_THandleStream;
    func_Create_TFileStream := CreateTwoParamFunctionType('afilename', ansiString64Type, 'mode', wordType, classType_TFileStream);
    TClassTypeDef(classType_TFileStream).AddMember('FileName', ansiString64Type);
    TClassTypeDef(classType_TFileStream).AddMember('Create', func_Create_TFileStream);
    TClassTypeDef(classType_TFileStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TFileStream).AddMember('Free', voidProcedureType);

    // TCustomMemoryStream
    classType_TCustomMemoryStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TCustomMemoryStream).parentClass := classType_TStream;
    TClassTypeDef(classType_TCustomMemoryStream).AddMember('Memory', pointer64Type);
    TClassTypeDef(classType_TCustomMemoryStream).AddMember('SaveToStream', proc_TStream);
    TClassTypeDef(classType_TCustomMemoryStream).AddMember('SaveToFile', proc_String);

    // TMemoryStream
    classType_TMemoryStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TMemoryStream).parentClass := classType_TCustomMemoryStream;
    func_Create_TMemoryStream := CreateFunctionType(TParameterList.Create, classType_TMemoryStream);
    TClassTypeDef(classType_TMemoryStream).AddMember('Create', func_Create_TMemoryStream);
    TClassTypeDef(classType_TMemoryStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TMemoryStream).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TMemoryStream).AddMember('Clear', voidProcedureType);
    TClassTypeDef(classType_TMemoryStream).AddMember('LoadFromStream', proc_TStream);
    TClassTypeDef(classType_TMemoryStream).AddMember('LoadFromFile', proc_String);
    TClassTypeDef(classType_TMemoryStream).AddMember('SetSize', proc_LongInt);

    // TStringStream
    classType_TStringStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TStringStream).parentClass := classType_TStream;
    func_Create_TStringStream := CreateOneParamFunctionType('astring', ansiString64Type, classType_TStringStream);
    TClassTypeDef(classType_TStringStream).AddMember('Create', func_Create_TStringStream);
    TClassTypeDef(classType_TStringStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TStringStream).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TStringStream).AddMember('DataString', ansiString64Type);
end;

procedure TClassesUnit.Load(ctx: TParserContext);
begin
    typesMock.Load(ctx);
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TPersistent', nil, skTypeName, classType_TPersistent, ctx.Cursor);
        RegisterSymbolByName('TComponent', nil, skTypeName, classType_TComponent, ctx.Cursor);
        RegisterSymbolByName('TFPList', nil, skTypeName, classType_TFPList, ctx.Cursor);
        RegisterSymbolByName('TStrings', nil, skTypeName, classType_TStrings, ctx.Cursor);
        RegisterSymbolByName('TStringList', nil, skTypeName, classType_TStringList, ctx.Cursor);
        RegisterSymbolByName('TStream', nil, skTypeName, classType_TStream, ctx.Cursor);
        RegisterSymbolByName('THandleStream', nil, skTypeName, classType_THandleStream, ctx.Cursor);
        RegisterSymbolByName('TFileStream', nil, skTypeName, classType_TFileStream, ctx.Cursor);
        RegisterSymbolByName('TCustomMemoryStream', nil, skTypeName, classType_TCustomMemoryStream, ctx.Cursor);
        RegisterSymbolByName('TMemoryStream', nil, skTypeName, classType_TMemoryStream, ctx.Cursor);
        RegisterSymbolByName('TStringStream', nil, skTypeName, classType_TStringStream, ctx.Cursor);

        RegisterSymbolByName('TSeekOrigin', nil, skTypeName, enumType_TSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soBeginning', nil, skConstant, memberTypeOfSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soCurrent', nil, skConstant, memberTypeOfSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soEnd', nil, skConstant, memberTypeOfSeekOrigin, ctx.Cursor);

        RegisterSymbolByName('TAlignment', nil, skTypeName, enumType_TAlignment, ctx.Cursor);
        RegisterSymbolByName('taLeftJustify', nil, skConstant, memberTypeOfAlignment, ctx.Cursor);
        RegisterSymbolByName('taRightJustify', nil, skConstant, memberTypeOfAlignment, ctx.Cursor);
        RegisterSymbolByName('taCenter', nil, skConstant, memberTypeOfAlignment, ctx.Cursor);

        RegisterSymbolByName('TListNotification', nil, skTypeName, enumType_TListNotification, ctx.Cursor);
        RegisterSymbolByName('lnAdded', nil, skConstant, memberTypeOfListNotification, ctx.Cursor);
        RegisterSymbolByName('lnExtracted', nil, skConstant, memberTypeOfListNotification, ctx.Cursor);
        RegisterSymbolByName('lnDeleted', nil, skConstant, memberTypeOfListNotification, ctx.Cursor);

        RegisterSymbolByName('TListAssignOp', nil, skTypeName, enumType_TListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laCopy', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laAnd', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laOr', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laXor', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laSrcUnique', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);
        RegisterSymbolByName('laDstUnique', nil, skConstant, memberTypeOfListAssignOp, ctx.Cursor);

        RegisterSymbolByName('TOperation', nil, skTypeName, enumType_TOperation, ctx.Cursor);
        RegisterSymbolByName('opInsert', nil, skConstant, memberTypeOfOperation, ctx.Cursor);
        RegisterSymbolByName('opRemove', nil, skConstant, memberTypeOfOperation, ctx.Cursor);

        RegisterSymbolByName('THelpType', nil, skTypeName, enumType_THelpType, ctx.Cursor);
        RegisterSymbolByName('htKeyword', nil, skConstant, memberTypeOfHelpType, ctx.Cursor);
        RegisterSymbolByName('htContext', nil, skConstant, memberTypeOfHelpType, ctx.Cursor);

        RegisterSymbolByName('TStreamSeekOrigin', nil, skTypeName, enumType_TStreamSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soFromBeginning', nil, skConstant, memberTypeOfStreamSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soFromCurrent', nil, skConstant, memberTypeOfStreamSeekOrigin, ctx.Cursor);
        RegisterSymbolByName('soFromEnd', nil, skConstant, memberTypeOfStreamSeekOrigin, ctx.Cursor);

        RegisterSymbolByName('TStringsSortStyle', nil, skTypeName, enumType_TStringsSortStyle, ctx.Cursor);
        RegisterSymbolByName('sssNone', nil, skConstant, memberTypeOfStringsSortStyle, ctx.Cursor);
        RegisterSymbolByName('sssUnSorted', nil, skConstant, memberTypeOfStringsSortStyle, ctx.Cursor);
        RegisterSymbolByName('sssSorted', nil, skConstant, memberTypeOfStringsSortStyle, ctx.Cursor);

        RegisterSymbolByName('TStringListSortStyle', nil, skTypeName, enumType_TStringListSortStyle, ctx.Cursor);
        RegisterSymbolByName('sslNone', nil, skConstant, memberTypeOfStringListSortStyle, ctx.Cursor);
        RegisterSymbolByName('sslAuto', nil, skConstant, memberTypeOfStringListSortStyle, ctx.Cursor);
        RegisterSymbolByName('sslUser', nil, skConstant, memberTypeOfStringListSortStyle, ctx.Cursor);

        RegisterSymbolByName('fmCreate', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmOpenRead', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmOpenWrite', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmOpenReadWrite', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmShareCompat', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmShareExclusive', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmShareDenyWrite', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmShareDenyRead', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('fmShareDenyNone', nil, skConstant, wordType, ctx.Cursor);
    end;
end;

end.
