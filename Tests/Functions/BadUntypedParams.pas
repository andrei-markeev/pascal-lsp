program BadUntypedParams;

procedure BadProc(x; const y);
begin
end;

{$mode iso}
procedure BadIsoProc(const z);
begin
end;

begin
end.
