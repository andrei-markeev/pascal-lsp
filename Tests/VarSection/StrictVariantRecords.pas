program StrictVariantRecords;

{$mode universalpascal}

type
    TNode = record
        case tag: integer of
            1: (x: integer);
            2: (y: real);
    end;

var
    n: TNode;

begin
    case n.tag of
        1: n.x := 10;
        2: n.y := 2.5;
    end;
end.
