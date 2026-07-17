program OrdinalFunctions;

{$mode objfpc}
{$longstrings on}

var
    i: integer;
    c: char;
    arr: array[1..10] of integer;
    h, l: integer;
begin
    i := 5;
    Inc(i);
    Inc(i, 2);
    Dec(i);
    Dec(i, 3);

    c := 'a';
    Inc(c);
    Dec(c);

    h := High(i);
    l := Low(i);
    h := High(c);
    l := Low(c);
    h := High(arr);
    l := Low(arr);

    h := High(integer);
    l := Low(integer);
    h := High(char);
    l := Low(char);
end.
