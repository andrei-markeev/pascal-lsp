unit BranchTracker;

{$mode objfpc}
{$longstrings on}

interface

uses
    sysutils, contnrs;

type
    TCaseLabel = record
        isRange: boolean;
        valStart: PChar;
        valLen: integer;
        highStart: PChar;
        highLen: integer;
    end;

    TCaseLabelArray = array of TCaseLabel;

    TCaseBranchContext = class
    public
        baseSymbol: TObject;
        tagSymbol: TObject;
        branchLabels: TCaseLabelArray;
        constructor Create(ABaseSymbol: TObject; ATagSymbol: TObject);
    end;

procedure PushCaseStatement(ABaseSymbol: TObject; ATagSymbol: TObject);
procedure ClearCurrentBranchLabels;
procedure PopCaseStatement;
function IsVariantFieldAccessAllowed(ABaseSymbol: TObject; ATagSymbol: TObject; const AVariantLabels: TCaseLabelArray): boolean;
procedure ResetBranchTracker;

function LabelMatches(const branchLabel, fieldLabel: TCaseLabel): boolean;
function LabelArrayMatches(const branchLabels, fieldLabels: TCaseLabelArray): boolean;
function CreateSingleLabel(AStart: PChar; ALen: integer): TCaseLabel;
function CreateRangeLabel(ALowStart: PChar; ALowLen: integer; AHighStart: PChar; AHighLen: integer): TCaseLabel;
procedure AddSingleLabel(AStart: PChar; ALen: integer);
procedure AddRangeLabel(ALowStart: PChar; ALowLen: integer; AHighStart: PChar; AHighLen: integer);

implementation

uses
    Symbols;

var
    BranchStack: array of TCaseBranchContext;

constructor TCaseBranchContext.Create(ABaseSymbol: TObject; ATagSymbol: TObject);
begin
    inherited Create;
    baseSymbol := ABaseSymbol;
    tagSymbol := ATagSymbol;
    SetLength(branchLabels, 0);
end;

function CreateSingleLabel(AStart: PChar; ALen: integer): TCaseLabel;
begin
    Result.isRange := false;
    Result.valStart := AStart;
    Result.valLen := ALen;
    Result.highStart := nil;
    Result.highLen := 0;
end;

function CreateRangeLabel(ALowStart: PChar; ALowLen: integer; AHighStart: PChar; AHighLen: integer): TCaseLabel;
begin
    Result.isRange := true;
    Result.valStart := ALowStart;
    Result.valLen := ALowLen;
    Result.highStart := AHighStart;
    Result.highLen := AHighLen;
end;

procedure AddSingleLabel(AStart: PChar; ALen: integer);
var
    topIdx, l: integer;
begin
    topIdx := Length(BranchStack) - 1;
    if topIdx >= 0 then
    begin
        l := Length(BranchStack[topIdx].branchLabels);
        SetLength(BranchStack[topIdx].branchLabels, l + 1);
        BranchStack[topIdx].branchLabels[l] := CreateSingleLabel(AStart, ALen);
    end;
end;

procedure AddRangeLabel(ALowStart: PChar; ALowLen: integer; AHighStart: PChar; AHighLen: integer);
var
    topIdx, l: integer;
begin
    topIdx := Length(BranchStack) - 1;
    if topIdx >= 0 then
    begin
        l := Length(BranchStack[topIdx].branchLabels);
        SetLength(BranchStack[topIdx].branchLabels, l + 1);
        BranchStack[topIdx].branchLabels[l] := CreateRangeLabel(ALowStart, ALowLen, AHighStart, AHighLen);
    end;
end;

function GetLabelStr(AStart: PChar; ALen: integer): string;
begin
    if (AStart = nil) or (ALen <= 0) then exit('');
    SetString(Result, AStart, ALen);
end;

function LabelMatches(const branchLabel, fieldLabel: TCaseLabel): boolean;
var
    bVal, fVal, lowVal, highVal, bLow, bHigh: int64;
    bValOk, fValOk, lowValOk, highValOk, bLowOk, bHighOk: boolean;
    bSingleStr, fSingleStr, bLowStr, bHighStr, fLowStr, fHighStr: string;
begin
    if not branchLabel.isRange and not fieldLabel.isRange then
    begin
        bSingleStr := GetLabelStr(branchLabel.valStart, branchLabel.valLen);
        fSingleStr := GetLabelStr(fieldLabel.valStart, fieldLabel.valLen);
        Result := LowerCase(bSingleStr) = LowerCase(fSingleStr);
        exit;
    end;

    if not branchLabel.isRange and fieldLabel.isRange then
    begin
        bSingleStr := GetLabelStr(branchLabel.valStart, branchLabel.valLen);
        fLowStr := GetLabelStr(fieldLabel.valStart, fieldLabel.valLen);
        fHighStr := GetLabelStr(fieldLabel.highStart, fieldLabel.highLen);

        bValOk := TryStrToInt64(bSingleStr, bVal);
        lowValOk := TryStrToInt64(fLowStr, lowVal);
        highValOk := TryStrToInt64(fHighStr, highVal);
        if bValOk and lowValOk and highValOk then
            Result := (bVal >= lowVal) and (bVal <= highVal)
        else
            Result := (LowerCase(bSingleStr) >= LowerCase(fLowStr)) and
                      (LowerCase(bSingleStr) <= LowerCase(fHighStr));
        exit;
    end;

    if branchLabel.isRange and fieldLabel.isRange then
    begin
        bLowStr := GetLabelStr(branchLabel.valStart, branchLabel.valLen);
        bHighStr := GetLabelStr(branchLabel.highStart, branchLabel.highLen);
        fLowStr := GetLabelStr(fieldLabel.valStart, fieldLabel.valLen);
        fHighStr := GetLabelStr(fieldLabel.highStart, fieldLabel.highLen);

        bLowOk := TryStrToInt64(bLowStr, bLow);
        bHighOk := TryStrToInt64(bHighStr, bHigh);
        lowValOk := TryStrToInt64(fLowStr, lowVal);
        highValOk := TryStrToInt64(fHighStr, highVal);
        if bLowOk and bHighOk and lowValOk and highValOk then
            Result := (bLow >= lowVal) and (bHigh <= highVal)
        else
            Result := (LowerCase(bLowStr) >= LowerCase(fLowStr)) and
                      (LowerCase(bHighStr) >= LowerCase(fHighStr));
        exit;
    end;

    // branchLabel is range, fieldLabel is single value
    bLowStr := GetLabelStr(branchLabel.valStart, branchLabel.valLen);
    bHighStr := GetLabelStr(branchLabel.highStart, branchLabel.highLen);
    fSingleStr := GetLabelStr(fieldLabel.valStart, fieldLabel.valLen);

    bLowOk := TryStrToInt64(bLowStr, bLow);
    bHighOk := TryStrToInt64(bHighStr, bHigh);
    fValOk := TryStrToInt64(fSingleStr, fVal);
    if bLowOk and bHighOk and fValOk then
        Result := (bLow = fVal) and (bHigh = fVal)
    else
        Result := (LowerCase(bLowStr) = LowerCase(fSingleStr)) and
                  (LowerCase(bHighStr) = LowerCase(fSingleStr));
end;

function LabelArrayMatches(const branchLabels, fieldLabels: TCaseLabelArray): boolean;
var
    i, j: integer;
    matched: boolean;
begin
    if Length(branchLabels) = 0 then
        exit(false);

    for i := 0 to Length(branchLabels) - 1 do
    begin
        matched := false;
        for j := 0 to Length(fieldLabels) - 1 do
        begin
            if LabelMatches(branchLabels[i], fieldLabels[j]) then
            begin
                matched := true;
                break;
            end;
        end;
        if not matched then
            exit(false);
    end;
    Result := true;
end;

procedure PushCaseStatement(ABaseSymbol: TObject; ATagSymbol: TObject);
var
    l: integer;
begin
    l := Length(BranchStack);
    SetLength(BranchStack, l + 1);
    BranchStack[l] := TCaseBranchContext.Create(ABaseSymbol, ATagSymbol);
end;

procedure ClearCurrentBranchLabels;
var
    l: integer;
begin
    l := Length(BranchStack);
    if l > 0 then
        SetLength(BranchStack[l - 1].branchLabels, 0);
end;

procedure PopCaseStatement;
var
    l: integer;
begin
    l := Length(BranchStack);
    if l > 0 then
    begin
        BranchStack[l - 1].Free;
        SetLength(BranchStack, l - 1);
    end;
end;

function IsVariantFieldAccessAllowed(ABaseSymbol: TObject; ATagSymbol: TObject; const AVariantLabels: TCaseLabelArray): boolean;
var
    i: integer;
    ctx: TCaseBranchContext;
begin
    Result := false;
    for i := Length(BranchStack) - 1 downto 0 do
    begin
        ctx := BranchStack[i];
        if (ctx.baseSymbol = ABaseSymbol) and (ctx.tagSymbol = ATagSymbol) then
        begin
            if LabelArrayMatches(ctx.branchLabels, AVariantLabels) then
                exit(true);
        end;
    end;
end;

procedure ResetBranchTracker;
var
    i: integer;
begin
    for i := 0 to Length(BranchStack) - 1 do
        BranchStack[i].Free;
    SetLength(BranchStack, 0);
end;

initialization
    SetLength(BranchStack, 0);

finalization
    ResetBranchTracker;

end.
