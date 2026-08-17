program BadStrictVariantRecords;

{$mode universalpascal}

type
    TBadNode = record
        case integer of
            1: (x: integer);
            2: (y: real);
    end;

    TNode = record
        case tag: integer of
            1: (x: integer);
            2: (y: real);
    end;

var
    n1, n2: TNode;

begin
    n1.x := 10;

    case n1.tag of
        1: n1.y := 2.5;
        2: n1.x := 5;
    end;

    case n2.tag of
        1: n1.x := 10;
    end;
end.
