unit TypesUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TTypesUnit = class(TSystemUnit)
    public
        enumType_TDuplicates: TTypeDef;
        memberTypeOfDuplicates: TTypeDef;
        enumType_TDirection: TTypeDef;
        memberTypeOfDirection: TTypeDef;

        recordType_TPoint: TTypeDef;
        pointerType_PPoint: TTypeDef;
        recordType_TRect: TTypeDef;
        pointerType_PRect: TTypeDef;
        recordType_TSize: TTypeDef;
        pointerType_PSize: TTypeDef;
        recordType_TSmallPoint: TTypeDef;

        dynArrayOfByteType: TTypeDef;
        dynArrayOfWordType: TTypeDef;
        dynArrayOfDWordType: TTypeDef;
        dynArrayOfIntegerType: TTypeDef;
        dynArrayOfCardinalType: TTypeDef;
        dynArrayOfInt64Type: TTypeDef;
        dynArrayOfQWordType: TTypeDef;
        dynArrayOfSingleType: TTypeDef;
        dynArrayOfDoubleType: TTypeDef;
        dynArrayOfBooleanType: TTypeDef;
        dynArrayOfStringType: TTypeDef;
        dynArrayOfWideStringType: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, CompilationMode, EnumTypeDef, EnumMemberTypeDef, RecordTypeDef, PointerTypeDef, DynamicArrayTypeDef;

destructor TTypesUnit.Destroy;
begin
    if loaded then
    begin
        enumType_TDuplicates.Free;
        memberTypeOfDuplicates.Free;
        enumType_TDirection.Free;
        memberTypeOfDirection.Free;

        recordType_TPoint.Free;
        pointerType_PPoint.Free;
        recordType_TRect.Free;
        pointerType_PRect.Free;
        recordType_TSize.Free;
        pointerType_PSize.Free;
        recordType_TSmallPoint.Free;

        dynArrayOfByteType.Free;
        dynArrayOfWordType.Free;
        dynArrayOfDWordType.Free;
        dynArrayOfIntegerType.Free;
        dynArrayOfCardinalType.Free;
        dynArrayOfInt64Type.Free;
        dynArrayOfQWordType.Free;
        dynArrayOfSingleType.Free;
        dynArrayOfDoubleType.Free;
        dynArrayOfBooleanType.Free;
        dynArrayOfStringType.Free;
        dynArrayOfWideStringType.Free;
    end;
    inherited Destroy;
end;

procedure TTypesUnit.InitTypes;
begin
    enumType_TDuplicates := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TDuplicates).AddMember('dupIgnore');
    TEnumTypeDef(enumType_TDuplicates).AddMember('dupAccept');
    TEnumTypeDef(enumType_TDuplicates).AddMember('dupError');
    memberTypeOfDuplicates := TEnumMemberTypeDef.Create(nil, enumType_TDuplicates, nil);

    enumType_TDirection := TEnumTypeDef.Create(nil, nil);
    TEnumTypeDef(enumType_TDirection).AddMember('FromBeginning');
    TEnumTypeDef(enumType_TDirection).AddMember('FromCurrent');
    TEnumTypeDef(enumType_TDirection).AddMember('FromEnd');
    memberTypeOfDirection := TEnumMemberTypeDef.Create(nil, enumType_TDirection, nil);

    recordType_TPoint := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_TPoint).AddMember('x', longintType);
    TRecordTypeDef(recordType_TPoint).AddMember('y', longintType);
    pointerType_PPoint := TPointerTypeDef.Create(nil, true, recordType_TPoint);

    recordType_TRect := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_TRect).AddMember('Left', longintType);
    TRecordTypeDef(recordType_TRect).AddMember('Top', longintType);
    TRecordTypeDef(recordType_TRect).AddMember('Right', longintType);
    TRecordTypeDef(recordType_TRect).AddMember('Bottom', longintType);
    pointerType_PRect := TPointerTypeDef.Create(nil, true, recordType_TRect);

    recordType_TSize := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_TSize).AddMember('cx', longintType);
    TRecordTypeDef(recordType_TSize).AddMember('cy', longintType);
    pointerType_PSize := TPointerTypeDef.Create(nil, true, recordType_TSize);

    recordType_TSmallPoint := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_TSmallPoint).AddMember('x', smallintType);
    TRecordTypeDef(recordType_TSmallPoint).AddMember('y', smallintType);

    dynArrayOfByteType := TDynamicArrayTypeDef.Create(nil, byteType, 8);
    dynArrayOfWordType := TDynamicArrayTypeDef.Create(nil, wordType, 8);
    dynArrayOfDWordType := TDynamicArrayTypeDef.Create(nil, longwordType, 8);
    dynArrayOfIntegerType := TDynamicArrayTypeDef.Create(nil, longintType, 8);
    dynArrayOfCardinalType := TDynamicArrayTypeDef.Create(nil, longwordType, 8);
    dynArrayOfInt64Type := TDynamicArrayTypeDef.Create(nil, int64Type, 8);
    dynArrayOfQWordType := TDynamicArrayTypeDef.Create(nil, qwordType, 8);
    dynArrayOfSingleType := TDynamicArrayTypeDef.Create(nil, singleType, 8);
    dynArrayOfDoubleType := TDynamicArrayTypeDef.Create(nil, doubleType, 8);
    dynArrayOfBooleanType := TDynamicArrayTypeDef.Create(nil, booleanType, 8);
    dynArrayOfStringType := TDynamicArrayTypeDef.Create(nil, ansiString64Type, 8);
    dynArrayOfWideStringType := TDynamicArrayTypeDef.Create(nil, ansiString64Type, 8);
end;

procedure TTypesUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TDuplicates', nil, skTypeName, enumType_TDuplicates, ctx.Cursor);
        RegisterSymbolByName('dupIgnore', nil, skConstant, memberTypeOfDuplicates, ctx.Cursor);
        RegisterSymbolByName('dupAccept', nil, skConstant, memberTypeOfDuplicates, ctx.Cursor);
        RegisterSymbolByName('dupError', nil, skConstant, memberTypeOfDuplicates, ctx.Cursor);

        RegisterSymbolByName('TDirection', nil, skTypeName, enumType_TDirection, ctx.Cursor);
        RegisterSymbolByName('FromBeginning', nil, skConstant, memberTypeOfDirection, ctx.Cursor);
        RegisterSymbolByName('FromCurrent', nil, skConstant, memberTypeOfDirection, ctx.Cursor);
        RegisterSymbolByName('FromEnd', nil, skConstant, memberTypeOfDirection, ctx.Cursor);

        RegisterSymbolByName('TPoint', nil, skTypeName, recordType_TPoint, ctx.Cursor);
        RegisterSymbolByName('PPoint', nil, skTypeName, pointerType_PPoint, ctx.Cursor);
        RegisterSymbolByName('TRect', nil, skTypeName, recordType_TRect, ctx.Cursor);
        RegisterSymbolByName('PRect', nil, skTypeName, pointerType_PRect, ctx.Cursor);
        RegisterSymbolByName('TSize', nil, skTypeName, recordType_TSize, ctx.Cursor);
        RegisterSymbolByName('PSize', nil, skTypeName, pointerType_PSize, ctx.Cursor);
        RegisterSymbolByName('TSmallPoint', nil, skTypeName, recordType_TSmallPoint, ctx.Cursor);

        RegisterSymbolByName('TByteDynArray', nil, skTypeName, dynArrayOfByteType, ctx.Cursor);
        RegisterSymbolByName('TWordDynArray', nil, skTypeName, dynArrayOfWordType, ctx.Cursor);
        RegisterSymbolByName('TDWordDynArray', nil, skTypeName, dynArrayOfDWordType, ctx.Cursor);
        RegisterSymbolByName('TIntegerDynArray', nil, skTypeName, dynArrayOfIntegerType, ctx.Cursor);
        RegisterSymbolByName('TCardinalDynArray', nil, skTypeName, dynArrayOfCardinalType, ctx.Cursor);
        RegisterSymbolByName('TInt64DynArray', nil, skTypeName, dynArrayOfInt64Type, ctx.Cursor);
        RegisterSymbolByName('TQWordDynArray', nil, skTypeName, dynArrayOfQWordType, ctx.Cursor);
        RegisterSymbolByName('TSingleDynArray', nil, skTypeName, dynArrayOfSingleType, ctx.Cursor);
        RegisterSymbolByName('TDoubleDynArray', nil, skTypeName, dynArrayOfDoubleType, ctx.Cursor);
        RegisterSymbolByName('TBooleanDynArray', nil, skTypeName, dynArrayOfBooleanType, ctx.Cursor);
        RegisterSymbolByName('TStringDynArray', nil, skTypeName, dynArrayOfStringType, ctx.Cursor);
        RegisterSymbolByName('TWideStringDynArray', nil, skTypeName, dynArrayOfWideStringType, ctx.Cursor);

        RegisterSymbolByName('TValueRelationship', nil, skTypeName, longintType, ctx.Cursor);
        RegisterSymbolByName('LessThanValue', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('EqualsValue', nil, skConstant, longintType, ctx.Cursor);
        RegisterSymbolByName('GreaterThanValue', nil, skConstant, longintType, ctx.Cursor);
    end;
end;

end.
