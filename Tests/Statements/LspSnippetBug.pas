program LspSnippetBug;

{$mode objfpc}

type
  TJSONData = class
  public
    function FindPath(const APath: string): TJSONData;
    function AsString: string;
  end;

procedure ProcessRequest;
var
  Json: TJSONData;
  Method: string;
begin
  try
    exit;
  except
    exit;
  end;

  try
    if Json.FindPath('method') <> nil then
      Method := Json.FindPath('method').AsString
    else
      Method := '';
  finally
  end;
end;

begin
end.
