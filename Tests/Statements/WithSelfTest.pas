program WithSelfTest;

{$mode objfpc}

type
    TTestWithSelf = class
    private
        secretVal: integer;
    protected
        protVal: integer;
    public
        pubVal: integer;
        procedure Test;
    end;

procedure TTestWithSelf.Test;
begin
    Self.secretVal := 10;
    Self.protVal := 20;
    with Self do
    begin
        secretVal := 1;
        protVal := 2;
        pubVal := 3;
    end;
end;

begin
end.
