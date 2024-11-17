unit SystemUnits;

interface

uses
    ParserContext;

procedure RegisterSystemSymbols(ctx: TParserContext);

implementation

uses
    CompilationMode, Symbols, TypeDefs, Parameters;

var
    functionType_Real: TTypeDef;
    functionType_String_Integer: TTypeDef;
    functionType_LongInt_LongInt: TTypeDef;
    functionType_Byte_Char: TTypeDef;
    functionType_LongInt_Boolean: TTypeDef;
    functionType_Real_Real: TTypeDef;
    functionType_Real_Longint: TTypeDef;
    functionType_constString_constString_LongInt: TTypeDef;
    procedureType_outString_PChar_LongInt: TTypeDef;

procedure RegisterSystemSymbols(ctx: TParserContext);
begin
    if ctx.mode >= cmStandardPascal then
    begin
        RegisterSymbolByName('Abs', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('ArcTan', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Chr', nil, skFunction, @functionType_Byte_Char, ctx.Cursor);
        RegisterSymbolByName('Cos', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        // TODO: Eof
        // TODO: Eoln
        RegisterSymbolByName('Exp', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Ln', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Odd', nil, skFunction, @functionType_LongInt_Boolean, ctx.Cursor);
        RegisterSymbolByName('Ord', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('Pred', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('Round', nil, skFunction, @functionType_Real_LongInt, ctx.Cursor);
        RegisterSymbolByName('Sin', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Sqr', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('Sqrt', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Succ', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('Trunc', nil, skFunction, @functionType_Real_LongInt, ctx.Cursor);

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
        // TODO: Writeln
    end;
    if ctx.mode >= cmTurboPascal then
    begin
        // Flow control procedures
        RegisterSymbolByName('Break', nil, skProcedure, @voidProcedureType, ctx.Cursor);
        RegisterSymbolByName('Continue', nil, skProcedure, @voidProcedureType, ctx.Cursor);
        // TODO: Exit 
        // TODO: Halt 
        // TODO: RunError 

        // Ordinal procedures & functions
        // TODO: Dec
        // TODO: Inc
        // TODO: High
        // TODO: Low

        // Arithmetic functions
        RegisterSymbolByName('Frac', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Int', nil, skFunction, @functionType_Real_Real, ctx.Cursor);
        RegisterSymbolByName('Pi', nil, skFunction, @functionType_Real, ctx.Cursor);

        // String procedures & functions
        // TODO: Concat
        // TODO: Copy
        // TODO: Delete
        // TODO: Insert
        RegisterSymbolByName('Length', nil, skFunction, @functionType_String_Integer, ctx.Cursor);
        RegisterSymbolByName('Pos', nil, skFunction, @functionType_constString_constString_LongInt, ctx.Cursor);
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
        RegisterSymbolByName('ErrorAddr', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('ExitCode', nil, skVariable, @smallintType, ctx.Cursor);
        RegisterSymbolByName('ExitProc', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('FileMode', nil, skVariable, @byteType, ctx.Cursor);
        RegisterSymbolByName('FreeList', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('FreeZero', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapEnd', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapError', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapOrg', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('HeapPtr', nil, skVariable, @pointer32Type, ctx.Cursor);
        // TODO: RegisterSymbolByName('Input', nil, skVariable, @textFileType, ctx.Cursor);
        RegisterSymbolByName('InOutRes', nil, skVariable, @smallintType, ctx.Cursor);
        // TODO: RegisterSymbolByName('Output', nil, skVariable, @textFileType, ctx.Cursor);
        RegisterSymbolByName('OvrCodeList', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrDebugPtr', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('OvrDosHandle', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrEmsHandle', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapEnd', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapOrg', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapPtr', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrHeapsize', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('OvrLoadList', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('Prefixseg', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('Randseed', nil, skVariable, @longintType, ctx.Cursor);
        RegisterSymbolByName('SaveIntOO', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt02', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveIntlB', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt21', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt23', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt24', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt34', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt35', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt36', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt37', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt38', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt39', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3A', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3B', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3C', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3D', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3E', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt3F', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('SaveInt75', nil, skVariable, @pointer32Type, ctx.Cursor);
        RegisterSymbolByName('seg0040', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('segAOOO', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('segBOOO', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('segB800', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('selectorInc', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('StackLimit', nil, skVariable, @wordType, ctx.Cursor);
        RegisterSymbolByName('Test8087', nil, skVariable, @byteType, ctx.Cursor);
    end;
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('SetString', nil, skProcedure, @procedureType_outString_PChar_LongInt, ctx.Cursor);
    end;
end;

function CreateFunctionType(params: TParameterList; returnType: PTypeDef): TTypeDef;
begin
    CreateFunctionType.kind := tkFunction;
    CreateFunctionType.visibility := vPublic;
    CreateFunctionType.size := 0;
    CreateFunctionType.parameters := params;
    CreateFunctionType.returnType := returnType;
end;

function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: PTypeDef): TTypeDef;
begin
    CreateOneParamFunctionType := CreateFunctionType(TParameterList.Create([CreateParam(ptkValue, paramName, paramType)]), returnType);
end;

function CreateProcedureType(params: TParameterList): TTypeDef;
begin
    CreateProcedureType.kind := tkProcedure;
    CreateProcedureType.visibility := vPublic;
    CreateProcedureType.size := 0;
    CreateProcedureType.parameters := params;
end;

procedure InitFunctionTypes;
begin

    functionType_Real := CreateFunctionType(TParameterList.Create, @realType);

    functionType_LongInt_LongInt := CreateOneParamFunctionType('v', @longintType, @longintType);
    functionType_String_Integer := CreateOneParamFunctionType('s', @ansiString64Type, @longintType);
    functionType_LongInt_Boolean := CreateOneParamFunctionType('v', @longintType, @booleanType);
    functionType_Byte_Char := CreateOneParamFunctionType('b', @byteType, @charType);
    functionType_Real_Real := CreateOneParamFunctionType('x', @realType, @realType);
    functionType_Real_Longint := CreateOneParamFunctionType('x', @realType, @longintType);

    functionType_constString_constString_LongInt := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkConst, 'substr', @shortstringType),
        CreateParam(ptkConst, 's', @shortstringType)
    ]), @longintType);

    procedureType_outString_PChar_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkOut, 's', @ansiString64Type),
        CreateParam(ptkValue, 'buf', @pcharType),
        CreateParam(ptkValue, 'l', @longintType)
    ]));

end;

initialization
    InitFunctionTypes;
end.
