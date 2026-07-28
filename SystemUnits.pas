unit SystemUnits;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef;

var
    classType_TObject: TTypeDef;

procedure RegisterSystemSymbols(ctx: TParserContext);
function LoadSystemUnit(unitName: string; ctx: TParserContext): boolean;

implementation

uses
    classes, contnrs, CompilationMode, Symbols, TypeDefs, Parameters, RoutineTypeDef,
    ArrayTypeDef, DynamicArrayTypeDef, ClassTypeDef,
    SystemUnit, ClassesUnit, ContnrsUnit, MathUnit, SysutilsUnit, StringsUnit;

procedure InitFunctionTypes; forward;

var
    functionType_Real: TTypeDef;
    functionType_String_Integer: TTypeDef;
    functionType_LongInt_LongInt: TTypeDef;
    functionType_Ordinal_LongInt: TTypeDef;
    functionType_Ordinal_Ordinal: TTypeDef;
    functionType_Byte_Char: TTypeDef;
    functionType_LongInt_Boolean: TTypeDef;
    functionType_Real_Real: TTypeDef;
    functionType_Real_Longint: TTypeDef;
    functionType_constString_constString_LongInt: TTypeDef;
    procedureType_outString_PChar_LongInt: TTypeDef;
    procedureType_Ordinal1: TTypeDef;
    procedureType_Ordinal2: TTypeDef;
    procedureType_Unknown: TTypeDef;
    procedureType_Void_Or_Unknown: TTypeDef;
    varargsProcedureType: TTypeDef;
    functionType_HighLow: TTypeDef;
    functionType_String_String: TTypeDef;
    functionType_Copy: TTypeDef;
    procedureType_SetLength: TTypeDef;
    functionType_Length_FPC: TTypeDef;
    functionType_DynArray_Integer: TTypeDef;
    functionType_Array_Integer: TTypeDef;
    func_Create_TObject: TTypeDef;

    classesMock: TClassesUnit;
    contnrsMock: TContnrsUnit;
    mathMock: TMathUnit;
    sysutilsMock: TSysutilsUnit;
    stringsMock: TStringsUnit;

procedure RegisterSystemSymbols(ctx: TParserContext);
begin
    if functionType_Real = nil then
        InitFunctionTypes;

    if ctx.mode >= cmStandardPascal then
    begin
        RegisterSymbolByName('True', nil, skConstant, booleanType, ctx.Cursor);
        RegisterSymbolByName('False', nil, skConstant, booleanType, ctx.Cursor);
        RegisterSymbolByName('Abs', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);
        RegisterSymbolByName('ArcTan', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Chr', nil, skFunction, functionType_Byte_Char, ctx.Cursor);
        RegisterSymbolByName('Cos', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        // TODO: Eof
        // TODO: Eoln
        RegisterSymbolByName('Exp', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Ln', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Odd', nil, skFunction, functionType_LongInt_Boolean, ctx.Cursor);
        RegisterSymbolByName('Ord', nil, skFunction, functionType_Ordinal_LongInt, ctx.Cursor);
        RegisterSymbolByName('Pred', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);
        RegisterSymbolByName('Round', nil, skFunction, functionType_Real_LongInt, ctx.Cursor);
        RegisterSymbolByName('Sin', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Sqr', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);
        RegisterSymbolByName('Sqrt', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Succ', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);
        RegisterSymbolByName('Trunc', nil, skFunction, functionType_Real_LongInt, ctx.Cursor);

        // TODO: Dispose
        // TODO: Get
        // TODO: New
        // TODO: Pack
        // TODO: Page
        // TODO: Put
        // TODO: Read
        // TODO: Readln
        // TODO: Reset
        // TODO: Rewrite
        // TODO: Unpack
        // TODO: Write
        RegisterSymbolByName('WriteLn', nil, skProcedure, varargsProcedureType, ctx.Cursor);
    end;
    if ctx.mode >= cmTurboPascal then
    begin
        // Flow control procedures
        RegisterSymbolByName('Break', nil, skProcedure, voidProcedureType, ctx.Cursor);
        RegisterSymbolByName('Continue', nil, skProcedure, voidProcedureType, ctx.Cursor);
        if ctx.mode = cmMacPascal then
            RegisterSymbolByName('Exit', nil, skProcedure, procedureType_Unknown, ctx.Cursor)
        else if ctx.mode = cmTurboPascal then
            RegisterSymbolByName('Exit', nil, skProcedure, voidProcedureType, ctx.Cursor)
        else
            RegisterSymbolByName('Exit', nil, skProcedure, procedureType_Void_Or_Unknown, ctx.Cursor);
        // TODO: Halt 
        // TODO: RunError 

        // Ordinal procedures & functions
        RegisterSymbolByName('Dec', nil, skProcedure, procedureType_Ordinal1, ctx.Cursor);
        RegisterSymbolByName('Inc', nil, skProcedure, procedureType_Ordinal1, ctx.Cursor);
        RegisterSymbolByName('High', nil, skFunction, functionType_HighLow, ctx.Cursor);
        RegisterSymbolByName('Low', nil, skFunction, functionType_HighLow, ctx.Cursor);

        // Arithmetic functions
        RegisterSymbolByName('Frac', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Int', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Pi', nil, skFunction, functionType_Real, ctx.Cursor);

        // String procedures & functions
        // TODO: Concat
        RegisterSymbolByName('Copy', nil, skFunction, functionType_Copy, ctx.Cursor);
        // TODO: Delete
        // TODO: Insert
        if ctx.mode >= cmFreePascal then
            RegisterSymbolByName('Length', nil, skFunction, functionType_Length_FPC, ctx.Cursor)
        else
            RegisterSymbolByName('Length', nil, skFunction, functionType_String_Integer, ctx.Cursor);
        RegisterSymbolByName('Pos', nil, skFunction, functionType_constString_constString_LongInt, ctx.Cursor);
        // TODO: Str
        // TODO: Val

        // Dynamic allocation procedures & functions
        // TODO: FreeMem 
        // TODO: GetMem 
        // TODO: MaxAvail 
        // TODO: MemAvail

        // Pointer and address functions
        // TODO: Addr 
        // TODO: Assigned 
        // TODO: CSeg 
        // TODO: DSeg 
        // TODO: Ofs 
        // TODO: Ptr 
        // TODO: Seg 
        // TODO: SPtr 
        // TODO: SSeg

        // Miscellaneous procedures & functions
        // TODO: Exclude
        // TODO: FillChar
        // TODO: Hi
        // TODO: Include
        // TODO: Lo
        // TODO: Move
        // TODO: ParamCount
        // TODO: ParamStr
        // TODO: Random
        // TODO: Randomize
        // TODO: SizeOf
        // TODO: Swap
        // TODO: TypeOf
        // TODO: UpCase

        // Predeclared variables in the System unit
        RegisterSymbolByName('ErrorAddr', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('ExitCode', nil, skVariable, smallintType, ctx.Cursor);
        RegisterSymbolByName('ExitProc', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('FileMode', nil, skVariable, byteType, ctx.Cursor);
        RegisterSymbolByName('FreeList', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('FreeZero', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapEnd', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapError', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapOrg', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapPtr', nil, skVariable, pointer32Type, ctx.Cursor);
        // TODO: RegisterSymbolByName('Input', nil, skVariable, textFileType, ctx.Cursor);
        RegisterSymbolByName('InOutRes', nil, skVariable, smallintType, ctx.Cursor);
        // TODO: RegisterSymbolByName('Output', nil, skVariable, textFileType, ctx.Cursor);
        RegisterSymbolByName('OvrCodeList', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrDebugPtr', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('OvrDosHandle', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrEmsHandle', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapEnd', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapOrg', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapPtr', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapsize', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('OvrLoadList', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('Prefixseg', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('Randseed', nil, skVariable, longintType, ctx.Cursor);
        RegisterSymbolByName('SaveIntOO', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt02', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveIntlB', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt21', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt23', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt24', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt34', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt35', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt36', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt37', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt38', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt39', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3A', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3B', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3C', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3D', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3E', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3F', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt75', nil, skVariable, pointer32Type, ctx.Cursor);
        RegisterSymbolByName('Seg0040', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('SegAOOO', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('SegBOOO', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('SegB800', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('SelectorInc', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('StackLimit', nil, skVariable, wordType, ctx.Cursor);
        RegisterSymbolByName('Test8087', nil, skVariable, byteType, ctx.Cursor);
    end;
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('SetString', nil, skProcedure, procedureType_outString_PChar_LongInt, ctx.Cursor);
        RegisterSymbolByName('LowerCase', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('SetLength', nil, skProcedure, procedureType_SetLength, ctx.Cursor);
        RegisterSymbolByName('TObject', nil, skTypeName, classType_TObject, ctx.Cursor);
    end;
end;

procedure InitFunctionTypes;
begin

    functionType_Real := CreateFunctionType(TParameterList.Create, realType);

    functionType_LongInt_LongInt := CreateOneParamFunctionType('v', longintType, longintType);
    functionType_Ordinal_LongInt := CreateOneParamFunctionType('v', unknownType, longintType);
    functionType_Ordinal_Ordinal := CreateOneParamFunctionType('v', unknownType, unknownType);
    functionType_String_Integer := CreateOneParamFunctionType('s', ansiString64Type, longintType);
    functionType_LongInt_Boolean := CreateOneParamFunctionType('v', longintType, booleanType);
    functionType_Byte_Char := CreateOneParamFunctionType('b', byteType, charType);
    functionType_Real_Real := CreateOneParamFunctionType('x', realType, realType);
    functionType_Real_Longint := CreateOneParamFunctionType('x', realType, longintType);

    functionType_constString_constString_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, 'substr', shortstringType),
        CreateParam(ptkConst, 's', shortstringType)
    ]), longintType);

    procedureType_outString_PChar_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkOut, 's', ansiString64Type),
        CreateParam(ptkValue, 'buf', pcharType),
        CreateParam(ptkValue, 'l', longintType)
    ]));

    procedureType_Ordinal1 := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'x', unknownType)
    ]));
    procedureType_Ordinal2 := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'x', unknownType),
        CreateParam(ptkValue, 'n', longintType)
    ]));
    if procedureType_Ordinal1 is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Ordinal1).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Ordinal1).overloads.Add(procedureType_Ordinal2);
    end;

    functionType_HighLow := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 'x', unknownType)
    ]), unknownType);

    procedureType_Unknown := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, 'v', unknownType)
    ]));
    procedureType_Void_Or_Unknown := CreateProcedureType(TParameterList.Create);
    if procedureType_Void_Or_Unknown is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Void_Or_Unknown).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Void_Or_Unknown).overloads.Add(procedureType_Unknown);
    end;
    varargsProcedureType := CreateProcedureType(nil);

    functionType_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);

    functionType_Copy := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', unknownType),
        CreateParam(ptkValue, 'index', longintType),
        CreateParam(ptkValue, 'count', longintType)
    ]), unknownType);

    procedureType_SetLength := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 's', unknownType),
        CreateParam(ptkValue, 'len', longintType)
    ]));

    functionType_DynArray_Integer := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', TDynamicArrayTypeDef.Create(nil, nil))
    ]), longintType);

    functionType_Array_Integer := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', TArrayTypeDef.Create(nil, nil, nil))
    ]), longintType);

    functionType_Length_FPC := CreateOneParamFunctionType('s', ansiString64Type, longintType);
    if functionType_Length_FPC is TRoutineTypeDef then
    begin
        TRoutineTypeDef(functionType_Length_FPC).overloads := TFPList.Create;
        TRoutineTypeDef(functionType_Length_FPC).overloads.Add(functionType_DynArray_Integer);
        TRoutineTypeDef(functionType_Length_FPC).overloads.Add(functionType_Array_Integer);
    end;

    classType_TObject := TClassTypeDef.Create;
    func_Create_TObject := CreateFunctionType(TParameterList.Create, classType_TObject);
    TClassTypeDef(classType_TObject).AddMember('Create', func_Create_TObject);
    TClassTypeDef(classType_TObject).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TObject).AddMember('Free', voidProcedureType);

end;

procedure FreeFunctionTypes;
begin
    functionType_Real.Free;
    functionType_String_Integer.Free;
    functionType_LongInt_LongInt.Free;
    functionType_Ordinal_LongInt.Free;
    functionType_Ordinal_Ordinal.Free;
    functionType_Byte_Char.Free;
    functionType_LongInt_Boolean.Free;
    functionType_Real_Real.Free;
    functionType_Real_Longint.Free;
    functionType_constString_constString_LongInt.Free;
    procedureType_outString_PChar_LongInt.Free;
    procedureType_Ordinal1.Free;
    procedureType_Ordinal2.Free;
    procedureType_Unknown.Free;
    procedureType_Void_Or_Unknown.Free;
    varargsProcedureType.Free;
    functionType_HighLow.Free;
    functionType_String_String.Free;
    functionType_Copy.Free;
    procedureType_SetLength.Free;
    functionType_Length_FPC.Free;
    functionType_DynArray_Integer.Free;
    functionType_Array_Integer.Free;
    func_Create_TObject.Free;
    classType_TObject.Free;
end;

function LoadSystemUnit(unitName: string; ctx: TParserContext): boolean;
begin
    if functionType_Real = nil then
        InitFunctionTypes;
    Result := true;
    case LowerCase(unitName) of
        'classes': classesMock.Load(ctx);
        'contnrs': contnrsMock.Load(ctx);
        'math': mathMock.Load(ctx);
        'sysutils': sysutilsMock.Load(ctx);
        'strings': stringsMock.Load(ctx);
        'system': ;
    else
        Result := false;
    end;
end;

procedure InitSystemUnits;
begin
    classesMock := TClassesUnit.Create;
    contnrsMock := TContnrsUnit.Create;
    mathMock := TMathUnit.Create;
    sysutilsMock := TSysutilsUnit.Create;
    stringsMock := TStringsUnit.Create;
end;

procedure FreeSystemUnits;
begin
    classesMock.Free;
    contnrsMock.Free;
    mathMock.Free;
    sysutilsMock.Free;
    stringsMock.Free;
end;

initialization
    InitSystemUnits;
finalization
    FreeFunctionTypes;
    FreeSystemUnits;
end.
