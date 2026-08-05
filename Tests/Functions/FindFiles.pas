program FindFiles;

{$mode objfpc}
{$longstrings on}

uses dos, sysutils;

var
    srDos: SearchRec;
    srSys: TSearchRec;
    res: integer;
    n: string;
    sz: longint;
    a: integer;
begin
    FindFirst('*.pas', AnyFile, srDos);
    FindNext(srDos);
    if DosError = 0 then
    begin
        n := srDos.Name;
        sz := srDos.Size;
        a := srDos.Attr;
    end;

    res := FindFirst('*.pas', faAnyFile or faReadOnly or faHidden or faSysFile or faVolumeID or faDirectory or faArchive, srSys);
    res := FindNext(srSys);
    if res = 0 then
    begin
        n := srSys.Name;
        sz := srSys.Size;
        a := srSys.Attr;
    end;
    FindClose(srSys);
end.
