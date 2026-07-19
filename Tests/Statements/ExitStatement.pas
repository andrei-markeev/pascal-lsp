program ExitStatement;

function TestFpc: integer;
begin
    TestFpc := 1;
    Exit;
    Exit(42);
end;

procedure TestTp;
{$mode tp}
begin
    Exit;
end;

procedure OuterProc;
    procedure InnerProc;
    {$mode macpas}
    begin
        Exit(OuterProc);
    end;
begin
    InnerProc;
end;

procedure OuterProcWithParams(a, b: integer);
    procedure InnerProc;
    {$mode macpas}
    begin
        Exit(OuterProcWithParams);
    end;
begin
    InnerProc;
end;

begin
    TestFpc;
    TestTp;
    OuterProc;
    OuterProcWithParams(1, 2);
end.
