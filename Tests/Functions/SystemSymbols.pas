program SystemSymbols;

{$mode objfpc}
{$longstrings on}

uses sysutils, strings;

var
    p: PChar;
    len: integer;
    buf: array[0..255] of byte;
    u: PtrUInt;
    i: PtrInt;
    qw: QWord;
begin
    p := 'Hello World';
    len := StrLen(p);
    FillChar(buf, SizeOf(buf), 0);
    qw := 12345;
    u := PtrUInt(p);
    i := PtrInt(qw);
    u := PtrUInt(u);
end.
