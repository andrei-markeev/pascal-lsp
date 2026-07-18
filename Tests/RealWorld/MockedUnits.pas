unit MockedUnits;

interface

uses
    classes, contnrs, math, sysutils, strings;

var
    list: TFPList;
    hashList: TFPHashList;
    strList: TStringList;
    cnt: integer;
    ptr: pointer;
    valMin: integer;
    str: string;
    p: PChar;
    b: boolean;
    idx: integer;

procedure TestMocked;

implementation

procedure TestMocked;
begin
    cnt := list.count;
    ptr := list.items[0];
    cnt := list.Add(ptr);
    list.Clear;

    ptr := hashList.find('key');
    cnt := hashList.Add('key', ptr);
    hashList.Delete(0);

    strList := TStringList.Create;
    strList.Add('test');
    strList.Delimiter := ';';
    strList.StrictDelimiter := true;
    strList.DelimitedText := 'a;b';
    cnt := strList.Count;
    idx := strList.IndexOf('test');
    str := strList[0];
    b := strList.Find('test', idx);
    strList.Clear;
    strList.Free;

    valMin := Min(10, 20);
    str := LowerCase('TEST');
    str := IntToStr(123);
    str := StringReplace('abc', 'a', 'b', []);
    if strlicomp(p, 'mode', 4) = 0 then
    begin
    end;
end;

end.
