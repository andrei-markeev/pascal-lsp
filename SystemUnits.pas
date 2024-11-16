unit SystemUnits;

interface

uses
    ParserContext;

procedure RegisterSystemSymbols(ctx: TParserContext);

implementation

uses
    CompilationMode, Symbols, TypeDefs, TypedToken;

var
    functionType_String_Integer: TTypeDef;
    procedureType_String_PChar_LongInt: TTypeDef;

procedure RegisterSystemSymbols(ctx: TParserContext);
begin
    if ctx.mode >= cmTurboPascal then
    begin
        RegisterSymbolByName('Length', nil, skFunction, @functionType_String_Integer, ctx.Cursor);
    end;
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('SetString', nil, skProcedure, @procedureType_String_PChar_LongInt, ctx.Cursor);
    end;
end;

procedure InitFunctionTypes;
var
    p: TTypedTokenArray;
    stringTypeToken: TTypedToken;
    pcharTypeToken: TTypedToken;
    longintTypeToken: TTypedToken;
begin
    stringTypeToken := TTypedToken.Create;
    stringTypeToken.typeDef := ansiString64Type;

    pcharTypeToken := TTypedToken.Create;
    pcharTypeToken.typeDef := pcharType;

    longintTypeToken := TTypedToken.Create;
    longintTypeToken.typeDef := longintType;

    functionType_String_Integer.kind := tkFunction;
    functionType_String_Integer.visibility := vPublic;
    functionType_String_Integer.size := 0;
    p := TTypedTokenArray.Create;
    p.Add(stringTypeToken);
    functionType_String_Integer.parameters := p;
    functionType_String_Integer.returnType := @smallintType;

    procedureType_String_PChar_LongInt.kind := tkProcedure;
    procedureType_String_PChar_LongInt.visibility := vPublic;
    procedureType_String_PChar_LongInt.size := 0;
    p := TTypedTokenArray.Create;
    p.Add(stringTypeToken);
    p.Add(pcharTypeToken);
    p.Add(longintTypeToken);
    procedureType_String_PChar_LongInt.parameters := p;
end;

initialization
    InitFunctionTypes;
end.
