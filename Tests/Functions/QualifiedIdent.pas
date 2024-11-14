program QualifiedIdent;

{$mode objfpc}

type
    TSomething = class
        actions: (aSayHello, aSayBye);
        procedure SayHello;
        procedure SayBye;
    end;

procedure TSomething.SayHello;
var s: string;
begin
    s := 'Hello world!';
end;

procedure TSomething.SayBye;
var s: string;
begin
    s := 'See you later';
end;

procedure TSomething;
var s: string;
begin
    s := 'Good morning';
end;

var
    something: TSomething;
begin
    // TODO: something.SayHello();
    // TODO: something.SayBye;
end.
