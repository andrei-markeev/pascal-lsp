unit MockedUnits;

interface

uses
    classes, contnrs, math, sysutils, strings;

var
    list: TFPList;
    hashList: TFPHashList;
    cnt: integer;
    ptr: pointer;
    valMin: integer;
    str: string;
    p: PChar;

procedure TestMocked;

implementation

procedure TestMocked;
begin
    cnt := list.count;
    ptr := list.items[0];
    ptr := hashList.find('key');
    valMin := Min(10, 20);
    str := LowerCase('TEST');
    str := IntToStr(123);
    str := StringReplace('abc', 'a', 'b', []);
    if strlicomp(p, 'mode', 4) = 0 then
    begin
    end;
end;

end.
