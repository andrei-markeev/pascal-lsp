unit JsonparserUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TJsonparserUnit = class(TSystemUnit)
    private
        enumType_TJSONOption: TTypeDef;
        classType_TJSONParser: TTypeDef;
        classType_EJSONParser: TTypeDef;

        func_Create_TJSONParser: TTypeDef;
        func_Parse_TJSONData: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, CompilationMode, Parameters, ClassTypeDef, EnumTypeDef, FpjsonUnit, SystemUnits;

destructor TJsonparserUnit.Destroy;
begin
    if loaded then
    begin
        enumType_TJSONOption.Free;
        classType_TJSONParser.Free;
        classType_EJSONParser.Free;

        func_Create_TJSONParser.Free;
        func_Parse_TJSONData.Free;
    end;
    inherited Destroy;
end;

procedure TJsonparserUnit.InitTypes;
var
    jsonDataType: TSymbol;
begin
    enumType_TJSONOption := TEnumTypeDef.Create(nil);
    TEnumTypeDef(enumType_TJSONOption).AddMember('joComments');
    TEnumTypeDef(enumType_TJSONOption).AddMember('joUseLCL');
    TEnumTypeDef(enumType_TJSONOption).AddMember('joUTF8');
    TEnumTypeDef(enumType_TJSONOption).AddMember('joStrict');

    classType_TJSONParser := TClassTypeDef.Create;
    TClassTypeDef(classType_TJSONParser).parentClass := classType_TObject;

    classType_EJSONParser := TClassTypeDef.Create;
    TClassTypeDef(classType_EJSONParser).parentClass := classType_TObject;

    jsonDataType := FindSymbol('TJSONData', nil);

    func_Create_TJSONParser := CreateFunctionType(TParameterList.Create([
        CreateParam(ptkValue, 'source', unknownType),
        CreateParam(ptkValue, 'options', unknownType, true)
    ]), classType_TJSONParser);

    if (jsonDataType <> nil) and (jsonDataType.typeDef <> nil) then
        func_Parse_TJSONData := CreateFunctionType(TParameterList.Create, jsonDataType.typeDef)
    else
        func_Parse_TJSONData := CreateFunctionType(TParameterList.Create, classType_TObject);

    TClassTypeDef(classType_TJSONParser).AddMember('Create', func_Create_TJSONParser);
    TClassTypeDef(classType_TJSONParser).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TJSONParser).AddMember('Free', voidProcedureType);
    TClassTypeDef(classType_TJSONParser).AddMember('Parse', func_Parse_TJSONData);
end;

procedure TJsonparserUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TJSONOption', nil, skTypeName, enumType_TJSONOption, ctx.Cursor);
        RegisterSymbolByName('joComments', nil, skConstant, enumType_TJSONOption, ctx.Cursor);
        RegisterSymbolByName('joUseLCL', nil, skConstant, enumType_TJSONOption, ctx.Cursor);
        RegisterSymbolByName('joUTF8', nil, skConstant, enumType_TJSONOption, ctx.Cursor);
        RegisterSymbolByName('joStrict', nil, skConstant, enumType_TJSONOption, ctx.Cursor);

        RegisterSymbolByName('TJSONParser', nil, skTypeName, classType_TJSONParser, ctx.Cursor);
        RegisterSymbolByName('EJSONParser', nil, skTypeName, classType_EJSONParser, ctx.Cursor);
    end;
end;

end.
