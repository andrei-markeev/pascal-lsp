program EmptySetConstructor;

{$mode objfpc}

type
    TCharSet = set of char;

const
    EmptyConstSet = [];
    TypedEmptySet: TCharSet = [];

var
    globalSet: TCharSet = [];

procedure Test(s: TCharSet = []);
begin
    s := [];
end;

begin
    globalSet := [];
end.
