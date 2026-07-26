program SystemFunctions;

{$mode objfpc}
{$longstrings on}

var
    s, s2: string;
    arr: array of integer;
    arrStatic: array[0..9] of char;
    len: integer;
begin
    s := 'Hello';
    s2 := LowerCase(s);
    SetLength(s2, 10);
    SetLength(arr, 5);
    s2 := Copy(s, 2, 3);
    len := Length(s);
    len := Length(arr);
    len := Length(arrStatic);
end.
