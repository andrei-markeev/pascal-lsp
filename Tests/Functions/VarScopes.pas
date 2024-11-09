program VarScopes;

var
    a: string;

procedure HelloWorld;
var
    a: integer;
begin
    a := 1;
end;

function TestFunc: integer;
begin
    a := 'hello!';
end;


begin
    a := 23;
end.