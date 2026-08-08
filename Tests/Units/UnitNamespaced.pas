program UnitNamespaced;

uses
    SysUtils;

var
    rec: SysUtils.TSearchRec;
    s: string;

begin
    s := SysUtils.LowerCase('HELLO');
    if SysUtils.PathDelim = '/' then
        s := SysUtils.IntToStr(123);
end.
