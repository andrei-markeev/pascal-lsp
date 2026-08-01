program ImplicitTObjectCreate;

{$mode objfpc}

type
    TToken = class
    public
        endMarker: TToken;
    end;

var
    endOf: TToken;
begin
    endOf := TToken.Create;
end.
