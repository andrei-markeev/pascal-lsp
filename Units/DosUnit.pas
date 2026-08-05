unit DosUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TDosUnit = class(TSystemUnit)
    private
        recordType_SearchRec: TTypeDef;
        procedureType_FindFirst: TTypeDef;
        procedureType_FindNext: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, Parameters, CompilationMode, RecordTypeDef;

destructor TDosUnit.Destroy;
begin
    if loaded then
    begin
        recordType_SearchRec.Free;
        procedureType_FindFirst.Free;
        procedureType_FindNext.Free;
    end;
    inherited Destroy;
end;

procedure TDosUnit.InitTypes;
begin
    recordType_SearchRec := TRecordTypeDef.Create(nil);
    TRecordTypeDef(recordType_SearchRec).AddMember('Fill', byteType);
    TRecordTypeDef(recordType_SearchRec).AddMember('Attr', byteType);
    TRecordTypeDef(recordType_SearchRec).AddMember('Time', longintType);
    TRecordTypeDef(recordType_SearchRec).AddMember('Size', longintType);
    TRecordTypeDef(recordType_SearchRec).AddMember('Name', ansiString64Type);

    procedureType_FindFirst := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkValue, 'path', ansiString64Type),
        CreateParam(ptkValue, 'attr', wordType),
        CreateParam(ptkVar, 's', recordType_SearchRec)
    ]));

    procedureType_FindNext := CreateProcedureType(TParameterList.Create([
        CreateParam(ptkVar, 's', recordType_SearchRec)
    ]));
end;

procedure TDosUnit.Load(ctx: TParserContext);
begin
    inherited Load(ctx);
    if (ctx.mode = cmTurboPascal) or (ctx.mode >= cmFreePascal) then
    begin
        RegisterSymbolByName('DosError', nil, skVariable, longintType, ctx.Cursor);

        RegisterSymbolByName('SearchRec', nil, skTypeName, recordType_SearchRec, ctx.Cursor);

        RegisterSymbolByName('ReadOnly', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('Hidden', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('SysFile', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('VolumeID', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('Directory', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('Archive', nil, skConstant, wordType, ctx.Cursor);
        RegisterSymbolByName('AnyFile', nil, skConstant, wordType, ctx.Cursor);

        RegisterSymbolByName('FindFirst', nil, skProcedure, procedureType_FindFirst, ctx.Cursor);
        RegisterSymbolByName('FindNext', nil, skProcedure, procedureType_FindNext, ctx.Cursor);
    end;
end;

end.
