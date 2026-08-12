{$mode universalpascal}
unit GoodPartialRecord;

interface

type
    TMyPartialRecord = partial record
        x: integer;
    end;

implementation

type
    TMyPartialRecord = partial record
        y: string;
    end;

procedure Test;
var
    r: TMyPartialRecord;
begin
    r.x := 10;
    r.y := 'hello';
end;

end.
