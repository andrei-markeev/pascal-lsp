program BadVariantRecordTypes;

type
    // Missing 'of' keyword
    TBad1 = record
        case kind: integer
            1: (x: integer);
    end;

    // Missing colon after case constant
    TBad2 = record
        case kind: integer of
            1 (y: real);
    end;

    // Missing open parenthesis
    TBad3 = record
        case kind: integer of
            1: z: integer);
    end;

    // Missing close parenthesis
    TBad4 = record
        case kind: integer of
            1: (w: integer;
    end;

    // Field after variant part (illegal)
    TBad5 = record
        case kind: integer of
            1: (w: integer);
        extraField: real;
    end;

begin
end.
