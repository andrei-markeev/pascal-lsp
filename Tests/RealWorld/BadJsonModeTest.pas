program BadJsonModeTest;

{$mode tp}

uses
    classes, fpjson, jsonparser;

var
    strm: TStream;
    jsonData: TJSONData;
    parser: TJSONParser;

begin
    strm := TStream.Create;
    jsonData := nil;
    parser := nil;
end.
