program BadEmptySetConstructor;

{$mode objfpc}

type
    TCharSet = set of char;

const
    BadSet: TCharSet = [;
    GoodSet: TCharSet = [];

begin
end.
