program PointerTypes;

{$mode objfpc}

type
    PInteger = ^integer;
    TNode = record
        value: integer;
        next: ^TNode;
    end;

var
    p: PInteger;
    val: integer;

begin
end.
