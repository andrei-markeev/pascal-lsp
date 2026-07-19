program BadUndeclaredProcCall;

var
    a, b: integer;

begin
    UndeclaredProc(a, b);
    b := 20;
end.
