program BadUnitNamespaced;

uses
    SysUtils;

var
    rec: SysUtils.NonExistentType;
    s: string;

begin
    s := SysUtils.NonExistentFunction('HELLO');
end.
