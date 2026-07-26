program SystemFunctions;

{$mode objfpc}
{$longstrings on}

var
    s, s2: string;
    arr: array of integer;
begin
    s := 'Hello';
    s2 := LowerCase(s);
    SetLength(s2, 10);
    SetLength(arr, 5);
    s2 := Copy(s, 2, 3);
end.
