program BadExitStatement;

procedure TestTpError;
{$mode tp}
begin
    Exit(123);
end;

procedure TestMacPasError;
{$mode macpas}
begin
    Exit;
end;

procedure TestTooManyArgs;
begin
    Exit(1, 2);
end;

procedure TestMacPasProcInExpr;
{$mode macpas}
var x: integer;
begin
    x := TestTpError + 1;
end;

begin
    TestTpError;
    TestMacPasError;
    TestTooManyArgs;
    TestMacPasProcInExpr;
end.
