program ChainedCall;

{$mode objfpc}

type
  THelper = class
  public
    procedure DoSomething;
  end;

  TMyClass = class
  public
    function GetHelper: THelper;
  end;

procedure THelper.DoSomething;
begin
end;

function TMyClass.GetHelper: THelper;
begin
  GetHelper := nil;
end;

var
  Obj: TMyClass;
begin
  Obj := TMyClass.Create;
  Obj.GetHelper().DoSomething;
end.
