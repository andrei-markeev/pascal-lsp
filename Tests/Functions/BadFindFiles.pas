program BadFindFiles;

{$mode objfpc}
{$longstrings on}

uses dos, sysutils;

var
    srDos: SearchRec;
    srSys: TSearchRec;
begin
    FindFirst('*.pas', AnyFile);
    FindNext;
    FindFirst;
    FindNext(srSys, 123);
end.
