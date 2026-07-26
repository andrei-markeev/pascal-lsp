program BadSystemFunctions;

{$mode objfpc}
{$longstrings on}

var
    s: string;
    arr: array of integer;
begin
    LowerCase();
    LowerCase(s, s);
    SetLength(s);
    SetLength(arr, 'hello');
    Copy(s);
    Copy(s, 1);
    Copy(s, 'hello', 3);
end.
