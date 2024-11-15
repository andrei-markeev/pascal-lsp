program PrivatePublic;

{$mode objfpc}

type
    TTestVisibility = class
        hello: string;
    private
        mySecret: string;
        function GetSecret: string;
    public
        constructor Create;
        procedure SayHello;
    end;

function TTestVisibility.GetSecret: string;
begin
    Result := mySecret;
end;

procedure TTestVisibility.SayHello;
var s: string;
begin
    hello := 'hello';
end;

constructor TTestVisibility.Create;
begin
    mySecret := '198nsadf93';
end;

function TTestVisibility.GetSecret: string;
begin
    Result := mySecret;
end;

var
    test: TTestVisibility;
    s: string;

begin
    test := TTestVisibility.Create;

    s := test.hello;
    test.SayHello;
    s := test.mySecret;
    s := test.GetSecret;
end.