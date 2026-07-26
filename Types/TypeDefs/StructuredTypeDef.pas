unit StructuredTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, TypeDef;

type
    TStructuredTypeDef = class(TTypeDef)
    private
        FNames: array of string;
        FTypes: array of TTypeDef;
        FHash: TFPHashList;
    public
        constructor Create(ctx: TTypeDefTracker = nil; AKind: TTypeKind = tkUnknown; ASize: longword = 0);
        destructor Destroy; override;
        procedure AddMember(const AName: string; AType: TTypeDef);
        function FindMember(const AName: string): TTypeDef;
        function GetMemberName(Index: integer): string;
        function GetMemberType(Index: integer): TTypeDef;
        function MemberCount: integer;
    end;

implementation

uses
    sysutils;

constructor TStructuredTypeDef.Create(ctx: TTypeDefTracker; AKind: TTypeKind; ASize: longword);
begin
    inherited Create(ctx, AKind, ASize);
    FHash := TFPHashList.Create;
    SetLength(FNames, 0);
    SetLength(FTypes, 0);
end;

destructor TStructuredTypeDef.Destroy;
begin
    FHash.Free;
    SetLength(FNames, 0);
    SetLength(FTypes, 0);
    inherited Destroy;
end;

procedure TStructuredTypeDef.AddMember(const AName: string; AType: TTypeDef);
var
    idx: integer;
begin
    idx := Length(FNames);
    SetLength(FNames, idx + 1);
    SetLength(FTypes, idx + 1);
    FNames[idx] := AName;
    FTypes[idx] := AType;
    FHash.Add(LowerCase(AName), Pointer(PtrUInt(idx + 1)));
end;

function TStructuredTypeDef.FindMember(const AName: string): TTypeDef;
var
    p: pointer;
begin
    p := FHash.Find(LowerCase(AName));
    if p <> nil then
        Result := FTypes[PtrUInt(p) - 1]
    else
        Result := nil;
end;

function TStructuredTypeDef.GetMemberName(Index: integer): string;
begin
    Result := FNames[Index];
end;

function TStructuredTypeDef.GetMemberType(Index: integer): TTypeDef;
begin
    Result := FTypes[Index];
end;

function TStructuredTypeDef.MemberCount: integer;
begin
    Result := Length(FNames);
end;

end.
