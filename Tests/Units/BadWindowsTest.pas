program BadWindowsTest;

{$mode objfpc}

uses windows;

var
    h: NonExistentType;

begin
    h := GetStdHandle(NonExistentConst);
    h := Windows.NonExistentFunc();
end.
