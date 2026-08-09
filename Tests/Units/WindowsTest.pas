program WindowsTest;

{$mode objfpc}

uses windows;

var
    hInput, hOutput: THandle;

begin
    hInput := GetStdHandle(STD_INPUT_HANDLE);
    hOutput := Windows.GetStdHandle(STD_OUTPUT_HANDLE);
end.
