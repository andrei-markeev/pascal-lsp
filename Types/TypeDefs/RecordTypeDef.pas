unit RecordTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef, StructuredTypeDef, BranchTracker;

type
    TVariantFieldInfo = record
        fieldSymbol: TObject;
        tagSymbol: TObject;
        labels: TCaseLabelArray;
    end;

    TRecordTypeDef = class(TStructuredTypeDef)
    private
        variantFields: array of TVariantFieldInfo;
    public
        isPartial: boolean;
        definingUnit: TObject;
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
        procedure AddVariantFieldInfo(fieldSymbol: TObject; tagSymbol: TObject; const labels: TCaseLabelArray);
        function IsVariantField(fieldSymbol: TObject; out tagSymbol: TObject; out labels: TCaseLabelArray): boolean;
    end;

function CheckPartialRecordInstantiation(ctx: TObject; typeDef: TTypeDef; token: TObject): boolean;

implementation

uses
    sysutils, ParserContext, Token, ArrayTypeDef, DynamicArrayTypeDef, Symbols;

constructor TRecordTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkRecord);
    isPartial := false;
    definingUnit := nil;
    SetLength(variantFields, 0);
end;

destructor TRecordTypeDef.Destroy;
begin
    SetLength(variantFields, 0);
    inherited Destroy;
end;

procedure TRecordTypeDef.AddVariantFieldInfo(fieldSymbol: TObject; tagSymbol: TObject; const labels: TCaseLabelArray);
var
    idx: integer;
begin
    idx := Length(variantFields);
    SetLength(variantFields, idx + 1);
    variantFields[idx].fieldSymbol := fieldSymbol;
    variantFields[idx].tagSymbol := tagSymbol;
    variantFields[idx].labels := labels;
end;

function TRecordTypeDef.IsVariantField(fieldSymbol: TObject; out tagSymbol: TObject; out labels: TCaseLabelArray): boolean;
var
    i: integer;
begin
    tagSymbol := nil;
    SetLength(labels, 0);
    if fieldSymbol <> nil then
    begin
        for i := 0 to Length(variantFields) - 1 do
        begin
            if variantFields[i].fieldSymbol = fieldSymbol then
            begin
                tagSymbol := variantFields[i].tagSymbol;
                labels := variantFields[i].labels;
                exit(true);
            end;
        end;
    end;
    Result := false;
end;

function GetBaseTypeDef(typeDef: TTypeDef): TTypeDef;
begin
    Result := typeDef;
    while Result <> nil do
    begin
        if (Result.kind = tkArray) and (Result is TArrayTypeDef) then
            Result := TArrayTypeDef(Result).typeOfValues
        else if (Result.kind = tkDynamicArray) and (Result is TDynamicArrayTypeDef) then
            Result := TDynamicArrayTypeDef(Result).typeOfDynValues
        else
            break;
    end;
end;

function CheckPartialRecordInstantiation(ctx: TObject; typeDef: TTypeDef; token: TObject): boolean;
var
    baseType: TTypeDef;
    pCtx: TParserContext;
    tok: TToken;
begin
    Result := true;
    baseType := GetBaseTypeDef(typeDef);
    if (baseType <> nil) and (baseType.kind = tkRecord) and TRecordTypeDef(baseType).isPartial then
    begin
        pCtx := TParserContext(ctx);
        tok := TToken(token);
        if (pCtx <> nil) and (pCtx.parseUnit <> nil) and (TRecordTypeDef(baseType).definingUnit <> nil) and
           (pCtx.parseUnit <> TRecordTypeDef(baseType).definingUnit) then
        begin
            if tok <> nil then
            begin
                tok.state := tsError;
                tok.errorMessage := 'Partial record cannot be instantiated outside of the defining unit!';
            end;
            Result := false;
        end;
    end;
end;

end.
