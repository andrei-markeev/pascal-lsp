unit ParamScopeShadowing;

interface

procedure Helper(pos: integer);

implementation

procedure Helper(pos: integer);
begin
end;

procedure TestCall;
var
    s, sub: string;
    idx: integer;
begin
    s := 'hello';
    sub := 'ell';
    idx := Pos(sub, s);
end;

end.
