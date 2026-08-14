unit TranspileRegister;

{$mode objfpc}
{$longstrings on}

interface

uses
    Token;

type
    TImplicitDeref = record
        pos: PChar;
        count: shortint;
    end;

    TOberonReceiver = record
        funcToken: TToken;
        receiverStart: PChar;
        receiverLen: integer;
        selfTypeName: string[32];
    end;

procedure ResetTranspileRegister;
procedure RegisterImplicitDeref(pos: PChar; count: shortint = 1);
procedure RegisterOberonReceiver(funcToken: TToken; receiverStart: PChar; receiverLen: integer; const selfTypeName: string);
function GetOberonReceiver(funcToken: TToken; out receiverStart: PChar; out receiverLen: integer; out selfTypeName: string): boolean;

var
    ImplicitDerefs: array of TImplicitDeref;
    ImplicitDerefsCount: integer = 0;
    OberonReceivers: array of TOberonReceiver;
    OberonReceiversCount: integer = 0;

implementation

procedure ResetTranspileRegister;
begin
    ImplicitDerefsCount := 0;
    SetLength(ImplicitDerefs, 0);
    OberonReceiversCount := 0;
    SetLength(OberonReceivers, 0);
end;

procedure RegisterImplicitDeref(pos: PChar; count: shortint = 1);
var
    i: integer;
begin
    for i := 0 to ImplicitDerefsCount - 1 do
    begin
        if ImplicitDerefs[i].pos = pos then
        begin
            inc(ImplicitDerefs[i].count, count);
            exit;
        end;
    end;

    if ImplicitDerefsCount >= length(ImplicitDerefs) then
        SetLength(ImplicitDerefs, (ImplicitDerefsCount + 1) * 2);

    ImplicitDerefs[ImplicitDerefsCount].pos := pos;
    ImplicitDerefs[ImplicitDerefsCount].count := count;
    inc(ImplicitDerefsCount);
end;

procedure RegisterOberonReceiver(funcToken: TToken; receiverStart: PChar; receiverLen: integer; const selfTypeName: string);
var
    i: integer;
begin
    for i := 0 to OberonReceiversCount - 1 do
    begin
        if OberonReceivers[i].funcToken = funcToken then
        begin
            OberonReceivers[i].receiverStart := receiverStart;
            OberonReceivers[i].receiverLen := receiverLen;
            OberonReceivers[i].selfTypeName := selfTypeName;
            exit;
        end;
    end;

    if OberonReceiversCount >= length(OberonReceivers) then
        SetLength(OberonReceivers, (OberonReceiversCount + 1) * 2);

    OberonReceivers[OberonReceiversCount].funcToken := funcToken;
    OberonReceivers[OberonReceiversCount].receiverStart := receiverStart;
    OberonReceivers[OberonReceiversCount].receiverLen := receiverLen;
    OberonReceivers[OberonReceiversCount].selfTypeName := selfTypeName;
    inc(OberonReceiversCount);
end;

function GetOberonReceiver(funcToken: TToken; out receiverStart: PChar; out receiverLen: integer; out selfTypeName: string): boolean;
var
    i: integer;
begin
    for i := 0 to OberonReceiversCount - 1 do
    begin
        if OberonReceivers[i].funcToken = funcToken then
        begin
            receiverStart := OberonReceivers[i].receiverStart;
            receiverLen := OberonReceivers[i].receiverLen;
            selfTypeName := OberonReceivers[i].selfTypeName;
            exit(true);
        end;
    end;
    receiverStart := nil;
    receiverLen := 0;
    selfTypeName := '';
    exit(false);
end;

finalization
    ResetTranspileRegister;

end.
