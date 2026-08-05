program SystemFunctions;

{$mode objfpc}
{$longstrings on}

uses sysutils;

var
    s, s2: string;
    arr: array of integer;
    arrStatic: array[0..9] of char;
    len: integer;
    b: boolean;
begin
    s := 'Hello';
    s2 := LowerCase(s);
    SetLength(s2, 10);
    SetLength(arr, 5);
    s2 := Copy(s, 2, 3);
    len := Length(s);
    len := Length(arr);
    len := Length(arrStatic);
    WriteLn;
    WriteLn(s, ' world', 123);
    Write('Testing write', 456);
    Readln;
    Readln(s);
    len := ParamCount;
    s2 := ParamStr(0);
    s2 := GetCurrentDir;
    b := FileExists('test.pas');
    b := DirectoryExists('Tests');
end.
