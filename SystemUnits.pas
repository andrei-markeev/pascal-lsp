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
    functionType_String_LongInt: TTypeDef;
    functionType_LongInt_LongInt: TTypeDef;
    functionType_Ordinal_LongInt: TTypeDef;
    functionType_Ordinal_Ordinal: TTypeDef;
    functionType_Byte_Char: TTypeDef;
    functionType_LongInt_Boolean: TTypeDef;
    functionType_Real_Real: TTypeDef;
    functionType_Real_LongInt: TTypeDef;
    functionType_constString_constString_LongInt: TTypeDef;
    procedureType_outString_PChar_LongInt: TTypeDef;
    procedureType_varOrdinal_LongInt: TTypeDef;
    procedureType_Unknown: TTypeDef;
    procedureType_Varargs: TTypeDef;
    functionType_String_String: TTypeDef;
    functionType_Unknown_LongInt_LongInt_Unknown: TTypeDef;
    functionType_DynArray_LongInt: TTypeDef;
    functionType_Array_LongInt: TTypeDef;
    functionType_TObject: TTypeDef;
    procedureType_File: TTypeDef;
    procedureType_File_String: TTypeDef;
    procedureType_File_LongInt: TTypeDef;
    procedureType_File_Unknown_LongInt: TTypeDef;
    procedureType_File_Unknown_LongInt_LongInt: TTypeDef;
    functionType_File_Boolean: TTypeDef;
    functionType_Boolean: TTypeDef;
    functionType_SmallInt: TTypeDef;
    functionType_LongInt: TTypeDef;
    functionType_LongInt_String: TTypeDef;
    functionType_File_LongInt: TTypeDef;

    // overloads
    procedureType_Ordinal1: TTypeDef;
    procedureType_Void_Or_Unknown: TTypeDef;
    functionType_Length_FPC: TTypeDef;
    procedureType_Reset_Rewrite_Mac: TTypeDef;
    procedureType_Reset_Rewrite_TP: TTypeDef;
    procedureType_BlockRead_BlockWrite: TTypeDef;
    functionType_Eof_Eoln: TTypeDef;

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
        RegisterSymbolByName('Eof', nil, skFunction, functionType_Eof_Eoln, ctx.Cursor);
        RegisterSymbolByName('Eoln', nil, skFunction, functionType_Eof_Eoln, ctx.Cursor);
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
        RegisterSymbolByName('Readln', nil, skProcedure, procedureType_Varargs, ctx.Cursor);
        if ctx.mode = cmMacPascal then
        begin
            RegisterSymbolByName('Reset', nil, skProcedure, procedureType_Reset_Rewrite_Mac, ctx.Cursor);
            RegisterSymbolByName('Rewrite', nil, skProcedure, procedureType_Reset_Rewrite_Mac, ctx.Cursor);
        end
        else if ctx.mode >= cmTurboPascal then
        begin
            RegisterSymbolByName('Reset', nil, skProcedure, procedureType_Reset_Rewrite_TP, ctx.Cursor);
            RegisterSymbolByName('Rewrite', nil, skProcedure, procedureType_Reset_Rewrite_TP, ctx.Cursor);
        end
        else
        begin
            RegisterSymbolByName('Reset', nil, skProcedure, procedureType_File, ctx.Cursor);
            RegisterSymbolByName('Rewrite', nil, skProcedure, procedureType_File, ctx.Cursor);
        end;
        // TODO: Unpack
        RegisterSymbolByName('Write', nil, skProcedure, procedureType_Varargs, ctx.Cursor);
        RegisterSymbolByName('WriteLn', nil, skProcedure, procedureType_Varargs, ctx.Cursor);

        RegisterSymbolByName('Input', nil, skVariable, textFileType, ctx.Cursor);
        RegisterSymbolByName('Output', nil, skVariable, textFileType, ctx.Cursor);
    end;
    if (ctx.mode = cmMacPascal) or (ctx.mode >= cmTurboPascal) then
    begin
        RegisterSymbolByName('Seek', nil, skProcedure, procedureType_File_LongInt, ctx.Cursor);
        RegisterSymbolByName('FilePos', nil, skFunction, functionType_File_LongInt, ctx.Cursor);
        RegisterSymbolByName('IOResult', nil, skFunction, functionType_SmallInt, ctx.Cursor);
        RegisterSymbolByName('Close', nil, skProcedure, procedureType_File, ctx.Cursor);
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
        RegisterSymbolByName('High', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);
        RegisterSymbolByName('Low', nil, skFunction, functionType_Ordinal_Ordinal, ctx.Cursor);

        // Arithmetic functions
        RegisterSymbolByName('Frac', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Int', nil, skFunction, functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Pi', nil, skFunction, functionType_Real, ctx.Cursor);

        // String procedures & functions
        // TODO: Concat
        RegisterSymbolByName('Copy', nil, skFunction, functionType_Unknown_LongInt_LongInt_Unknown, ctx.Cursor);
        // TODO: Delete
        // TODO: Insert
        if ctx.mode >= cmFreePascal then
            RegisterSymbolByName('Length', nil, skFunction, functionType_Length_FPC, ctx.Cursor)
        else
            RegisterSymbolByName('Length', nil, skFunction, functionType_String_LongInt, ctx.Cursor);
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
        // TODO: Random
        // TODO: Randomize
        // TODO: SizeOf
        // TODO: Swap
        // TODO: TypeOf
        // TODO: UpCase

        // File procedures & functions
        RegisterSymbolByName('Assign', nil, skProcedure, procedureType_File_String, ctx.Cursor);
        RegisterSymbolByName('FileSize', nil, skFunction, functionType_File_LongInt, ctx.Cursor);
        RegisterSymbolByName('Rename', nil, skProcedure, procedureType_File_String, ctx.Cursor);
        RegisterSymbolByName('Erase', nil, skProcedure, procedureType_File, ctx.Cursor);
        RegisterSymbolByName('Truncate', nil, skProcedure, procedureType_File, ctx.Cursor);
        RegisterSymbolByName('Flush', nil, skProcedure, procedureType_File, ctx.Cursor);
        RegisterSymbolByName('BlockRead', nil, skProcedure, procedureType_BlockRead_BlockWrite, ctx.Cursor);
        RegisterSymbolByName('BlockWrite', nil, skProcedure, procedureType_BlockRead_BlockWrite, ctx.Cursor);

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
    if (ctx.mode = cmExtendedPascal) or (ctx.mode = cmTurboPascal) or (ctx.mode >= cmFreePascal) then
    begin
        RegisterSymbolByName('ParamCount', nil, skFunction, functionType_LongInt, ctx.Cursor);
        RegisterSymbolByName('ParamStr', nil, skFunction, functionType_LongInt_String, ctx.Cursor);
    end;
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('AssignFile', nil, skProcedure, procedureType_File_String, ctx.Cursor);
        RegisterSymbolByName('CloseFile', nil, skProcedure, procedureType_File, ctx.Cursor);
        RegisterSymbolByName('SetString', nil, skProcedure, procedureType_outString_PChar_LongInt, ctx.Cursor);
        RegisterSymbolByName('LowerCase', nil, skFunction, functionType_String_String, ctx.Cursor);
        RegisterSymbolByName('SetLength', nil, skProcedure, procedureType_varOrdinal_LongInt, ctx.Cursor);
        RegisterSymbolByName('TObject', nil, skTypeName, classType_TObject, ctx.Cursor);
    end;
end;

procedure InitFunctionTypes;
begin

    functionType_Real := CreateFunctionType(TParameterList.Create, realType);

    functionType_LongInt_LongInt := CreateOneParamFunctionType('v', longintType, longintType);
    functionType_Ordinal_LongInt := CreateOneParamFunctionType('v', unknownType, longintType);
    functionType_Ordinal_Ordinal := CreateOneParamFunctionType('v', unknownType, unknownType);
    functionType_String_LongInt := CreateOneParamFunctionType('s', ansiString64Type, longintType);
    functionType_LongInt_Boolean := CreateOneParamFunctionType('v', longintType, booleanType);
    functionType_Byte_Char := CreateOneParamFunctionType('b', byteType, charType);
    functionType_Real_Real := CreateOneParamFunctionType('x', realType, realType);
    functionType_Real_LongInt := CreateOneParamFunctionType('x', realType, longintType);

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
    procedureType_varOrdinal_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'x', unknownType),
        CreateParam(ptkValue, 'n', longintType)
    ]));
    if procedureType_Ordinal1 is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Ordinal1).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Ordinal1).overloads.Add(procedureType_varOrdinal_LongInt);
    end;

    procedureType_Unknown := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, 'v', unknownType)
    ]));
    procedureType_Void_Or_Unknown := CreateProcedureType(TParameterList.Create);
    if procedureType_Void_Or_Unknown is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Void_Or_Unknown).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Void_Or_Unknown).overloads.Add(procedureType_Unknown);
    end;
    procedureType_Varargs := CreateProcedureType(nil);

    functionType_String_String := CreateOneParamFunctionType('s', ansiString64Type, ansiString64Type);

    functionType_Unknown_LongInt_LongInt_Unknown := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', unknownType),
        CreateParam(ptkValue, 'index', longintType),
        CreateParam(ptkValue, 'count', longintType)
    ]), unknownType);

    functionType_DynArray_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', TDynamicArrayTypeDef.Create(nil, nil))
    ]), longintType);

    functionType_Array_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 's', TArrayTypeDef.Create(nil, nil, nil))
    ]), longintType);

    functionType_Length_FPC := CreateOneParamFunctionType('s', ansiString64Type, longintType);
    if functionType_Length_FPC is TRoutineTypeDef then
    begin
        TRoutineTypeDef(functionType_Length_FPC).overloads := TFPList.Create;
        TRoutineTypeDef(functionType_Length_FPC).overloads.Add(functionType_DynArray_LongInt);
        TRoutineTypeDef(functionType_Length_FPC).overloads.Add(functionType_Array_LongInt);
    end;

    classType_TObject := TClassTypeDef.Create;
    functionType_TObject := CreateFunctionType(TParameterList.Create, classType_TObject);
    TClassTypeDef(classType_TObject).AddMember('Create', functionType_TObject);
    TClassTypeDef(classType_TObject).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TObject).AddMember('Free', voidProcedureType);

    procedureType_File := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType)
    ]));
    procedureType_File_String := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType),
        CreateParam(ptkConst, 'name', shortstringType)
    ]));
    procedureType_File_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType),
        CreateParam(ptkValue, 'recsize', longintType)
    ]));

    procedureType_Reset_Rewrite_Mac := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType)
    ]));
    if procedureType_Reset_Rewrite_Mac is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Reset_Rewrite_Mac).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Reset_Rewrite_Mac).overloads.Add(procedureType_File_String);
    end;

    procedureType_Reset_Rewrite_TP := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType)
    ]));
    if procedureType_Reset_Rewrite_TP is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_Reset_Rewrite_TP).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_Reset_Rewrite_TP).overloads.Add(procedureType_File_LongInt);
    end;

    functionType_File_Boolean := CreateOneParamFunctionType('f', fileType, booleanType);
    functionType_Boolean := CreateFunctionType(TParameterList.Create, booleanType);
    functionType_Eof_Eoln := CreateOneParamFunctionType('f', fileType, booleanType);
    if functionType_Eof_Eoln is TRoutineTypeDef then
    begin
        TRoutineTypeDef(functionType_Eof_Eoln).overloads := TFPList.Create;
        TRoutineTypeDef(functionType_Eof_Eoln).overloads.Add(functionType_Boolean);
    end;

    functionType_File_LongInt := CreateOneParamFunctionType('f', fileType, longintType);
    functionType_SmallInt := CreateFunctionType(TParameterList.Create, smallintType);
    functionType_LongInt := CreateFunctionType(TParameterList.Create, longintType);
    functionType_LongInt_String := CreateOneParamFunctionType('l', longintType, ansiString64Type);

    procedureType_File_Unknown_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType),
        CreateParam(ptkVar, 'buf', unknownType),
        CreateParam(ptkValue, 'count', longintType)
    ]));
    procedureType_File_Unknown_LongInt_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType),
        CreateParam(ptkVar, 'buf', unknownType),
        CreateParam(ptkValue, 'count', longintType),
        CreateParam(ptkVar, 'resultcount', longintType)
    ]));
    procedureType_BlockRead_BlockWrite := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 'f', fileType),
        CreateParam(ptkVar, 'buf', unknownType),
        CreateParam(ptkValue, 'count', longintType)
    ]));
    if procedureType_BlockRead_BlockWrite is TRoutineTypeDef then
    begin
        TRoutineTypeDef(procedureType_BlockRead_BlockWrite).overloads := TFPList.Create;
        TRoutineTypeDef(procedureType_BlockRead_BlockWrite).overloads.Add(procedureType_File_Unknown_LongInt_LongInt);
    end;

end;

procedure FreeFunctionTypes;
begin
    functionType_Real.Free;
    functionType_String_LongInt.Free;
    functionType_LongInt_LongInt.Free;
    functionType_Ordinal_LongInt.Free;
    functionType_Ordinal_Ordinal.Free;
    functionType_Byte_Char.Free;
    functionType_LongInt_Boolean.Free;
    functionType_Real_Real.Free;
    functionType_Real_LongInt.Free;
    functionType_constString_constString_LongInt.Free;
    procedureType_outString_PChar_LongInt.Free;
    procedureType_varOrdinal_LongInt.Free;
    procedureType_Unknown.Free;
    procedureType_Void_Or_Unknown.Free;
    procedureType_Varargs.Free;
    functionType_String_String.Free;
    functionType_Unknown_LongInt_LongInt_Unknown.Free;
    functionType_Length_FPC.Free;
    functionType_DynArray_LongInt.Free;
    functionType_Array_LongInt.Free;
    functionType_TObject.Free;
    classType_TObject.Free;

    procedureType_File.Free;
    procedureType_File_String.Free;
    procedureType_File_LongInt.Free;
    procedureType_Reset_Rewrite_Mac.Free;
    procedureType_Reset_Rewrite_TP.Free;
    procedureType_File_Unknown_LongInt.Free;
    procedureType_File_Unknown_LongInt_LongInt.Free;
    procedureType_BlockRead_BlockWrite.Free;
    functionType_File_Boolean.Free;
    functionType_Boolean.Free;
    functionType_Eof_Eoln.Free;
    functionType_File_LongInt.Free;
    functionType_SmallInt.Free;
    functionType_LongInt.Free;
    functionType_LongInt_String.Free;
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
