{$mode universalpascal}
unit BadPartialRecordDuplicateField;

interface

type
    TMyPartialRecord = partial record
        x: integer;
    end;

implementation

type
    TMyPartialRecord = partial record
        x: integer;
    end;

end.
