unit ClassesUnit;

{$mode objfpc}

interface

uses
    ParserContext, SystemUnit, TypeDefs;

type
    TClassesUnit = class(TSystemUnit)
    private
        classType_TFPList: TTypeDef;
        dynArrayOfPointerType: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    contnrs, Symbols, CompilationMode;

destructor TClassesUnit.Destroy;
begin
    if loaded then
        classType_TFPList.classFields.Free;
    inherited Destroy;
end;

procedure TClassesUnit.InitTypes;
begin
    dynArrayOfPointerType.kind := tkDynamicArray;
    dynArrayOfPointerType.size := 8;
    dynArrayOfPointerType.typeOfDynValues := @pointer64Type;

    classType_TFPList.kind := tkClass;
    classType_TFPList.size := 0;
    classType_TFPList.classFields := TFPHashList.Create;
    classType_TFPList.parentClass := nil;
    classType_TFPList.classFields.Add('count', @longintType);
    classType_TFPList.classFields.Add('items', @dynArrayOfPointerType);
end;

procedure TClassesUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if ctx.mode >= cmFreePascal then
    begin
        RegisterSymbolByName('TFPList', nil, skTypeName, @classType_TFPList, ctx.Cursor);
    end;
end;

end.
