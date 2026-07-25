program BadChainedCall;

{$mode tp}

type
  TMyRecord = record
    Field: integer;
  end;

function GetRecord: TMyRecord;
begin
end;

begin
  // Invalid member access on function call in Turbo Pascal mode:
  GetRecord().Field := 1;

  // Invalid assignment to function call:
  GetRecord() := 1;
end.
