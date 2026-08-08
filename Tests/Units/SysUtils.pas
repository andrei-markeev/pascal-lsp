program SysUtils;

{$mode objfpc}
{$longstrings on}

uses sysutils;

var
    s, s2: string;
    n: integer;
begin
    s := '  hello world  ';
    s2 := Trim(s);
    s2 := TrimLeft(s);
    s2 := TrimRight(s);
    n := StrToIntDef('123', 0);
    n := StrToIntDef('invalid', 42);
    n := StrToInt('456');
end.
