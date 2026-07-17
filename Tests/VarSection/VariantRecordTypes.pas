program VariantRecordTypes;

type
    TNode1 = record
        case kind: integer of
            1: (x: integer);
            2: (y: real);
    end;

    TNode2 = record
        case integer of
            1: (y: real);
    end;

    TNode3 = record
        a: integer;
        case b: boolean of
            true: (
                c: real;
                case d: integer of
                    1: (e: char);
                    2: (f: boolean; g: integer);
            );
            false: (
                h: integer
            );
    end;

var
    n: TNode3;

begin
    n.a := 1;
    n.c := 2.5;
    n.e := 'A';
    n.h := 10;
end.
