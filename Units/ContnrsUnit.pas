unit ContnrsUnit;

{$mode objfpc}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TContnrsUnit = class(TSystemUnit)
    private
        classType_TFPHashList: TTypeDef;
        functionType_constString_Pointer: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    contnrs, Symbols, CompilationMode;

destructor TContnrsUnit.Destroy;
begin
    if loaded then
        classType_TFPHashList.classFields.Free;
    inherited Destroy;
end;

procedure TContnrsUnit.InitTypes;
begin
    classType_TFPHashList.kind := tkClass;
    classType_TFPHashList.size := 0;
    classType_TFPHashList.classFields := TFPHashList.Create;
    classType_TFPHashList.parentClass := nil;

    functionType_constString_Pointer := CreateOneParamFunctionType('s', @shortstringType, @pointer64Type);
    classType_TFPHashList.classFields.Add('find', @functionType_constString_Pointer);
end;

procedure TContnrsUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPHashList', nil, skTypeName, @classType_TFPHashList, ctx.Cursor);
    end;
end;

end.
