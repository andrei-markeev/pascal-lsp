unit SystemUnits;

interface

uses
    ParserContext;

procedure RegisterSystemSymbols(ctx: TParserContext);

implementation

uses
    CompilationMode, Symbols, TypeDefs, Parameters;

var
    functionType_String_Integer: TTypeDef;
    functionType_LongInt_LongInt: TTypeDef;
    functionType_Byte_Char: TTypeDef;
    functionType_LongInt_Boolean: TTypeDef;
    procedureType_String_PChar_LongInt: TTypeDef;

procedure RegisterSystemSymbols(ctx: TParserContext);
begin
    if ctx.mode >= cmStandardPascal then
    begin
        RegisterSymbolByName('Abs', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        // TODO: ArcTan
        RegisterSymbolByName('Chr', nil, skFunction, @functionType_Byte_Char, ctx.Cursor);
        // TODO: Cos
        // TODO: Eof
        // TODO: Eoln
        // TODO: Exp
        // TODO: Ln
        // TODO: Odd
        RegisterSymbolByName('Ord', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        RegisterSymbolByName('Pred', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        // TODO: Round
        // TODO: Sin
        // TODO: Sqr
        // TODO: Sqrt
        RegisterSymbolByName('Succ', nil, skFunction, @functionType_LongInt_LongInt, ctx.Cursor);
        // TODO: Trunc

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
        // TODO: Frac
        // TODO: Int
        // TODO: Pi

        // String procedures & functions
        // TODO: Concat
        // TODO: Copy
        // TODO: Delete
        // TODO: Insert
        RegisterSymbolByName('Length', nil, skFunction, @functionType_String_Integer, ctx.Cursor);
        // TODO: Pos
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
    end;
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('SetString', nil, skProcedure, @procedureType_String_PChar_LongInt, ctx.Cursor);
    end;
end;

function CreateOneParamFunctionType(paramName: shortstring; paramType, returnType: PTypeDef): TTypeDef;
var
    params: TParameterList;
begin
    CreateOneParamFunctionType.kind := tkFunction;
    CreateOneParamFunctionType.visibility := vPublic;
    CreateOneParamFunctionType.size := 0;
    params := TParameterList.Create;
    params.Add(CreateParam(ptkValue, paramName, paramType));
    CreateOneParamFunctionType.parameters := params;
    CreateOneParamFunctionType.returnType := returnType;
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

    functionType_LongInt_LongInt := CreateOneParamFunctionType('v', @longintType, @longintType);
    functionType_String_Integer := CreateOneParamFunctionType('s', @ansiString64Type, @longintType);
    functionType_LongInt_Boolean := CreateOneParamFunctionType('v', @longintType, @booleanType);
    functionType_Byte_Char := CreateOneParamFunctionType('b', @byteType, @charType);

    procedureType_String_PChar_LongInt := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkOut, 's', @ansiString64Type),
        CreateParam(ptkValue, 'buf', @pcharType),
        CreateParam(ptkValue, 'l', @longintType)
    ]));
    
end;

initialization
    InitFunctionTypes;
end.
