program UntypedParams;

procedure CustomMove(const Source; var Dest; Count: integer);
begin
    Dest := Source;
end;

var
    s, d: integer;
begin
    CustomMove(s, d, 4);
end.
