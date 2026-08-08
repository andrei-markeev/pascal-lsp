program Classes;

{$mode objfpc}
{$longstrings on}

uses types, classes;

var
    hs: THandleStream;
    h: integer;
    sl: TStringList;
    d: TDuplicates;
    sss: TStringsSortStyle;
    ssl: TStringListSortStyle;
    pt: TPoint;
    fs: TFileStream;
    ms: TMemoryStream;
begin
    hs := THandleStream.Create(1);
    h := hs.Handle;
    hs.Free;

    sl := TStringList.Create;
    sl.Duplicates := dupIgnore;
    d := sl.Duplicates;
    sss := sl.SortStyle;
    ssl := sslAuto;
    sl.Free;

    pt.x := 10;
    pt.y := 20;

    fs := TFileStream.Create('test.txt', fmOpenRead);
    fs.Free;

    ms := TMemoryStream.Create;
    ms.Clear;
    ms.Free;
end.
