unit RecordTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef, StructuredTypeDef;

type
    TRecordTypeDef = class(TStructuredTypeDef)
    public
        isPartial: boolean;
        definingUnit: TObject;
        constructor Create(ctx: TTypeDefTracker = nil);
        destructor Destroy; override;
    end;

function CheckPartialRecordInstantiation(ctx: TObject; typeDef: TTypeDef; token: TObject): boolean;

implementation

uses
    ParserContext, Token, ArrayTypeDef, DynamicArrayTypeDef;

constructor TRecordTypeDef.Create(ctx: TTypeDefTracker);
begin
    inherited Create(ctx, tkRecord);
    isPartial := false;
    definingUnit := nil;
end;

destructor TRecordTypeDef.Destroy;
begin
    inherited Destroy;
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
