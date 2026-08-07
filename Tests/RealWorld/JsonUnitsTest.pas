unit JsonUnitsTest;

{$mode objfpc}

interface

uses
    classes, fpjson, jsonparser;

procedure TestJsonUnits;

implementation

procedure TestJsonUnits;
var
    strm: TStream;
    jsonData: TJSONData;
    jsonObj: TJSONObject;
    jsonArr: TJSONArray;
    parser: TJSONParser;
    valType: TJSONtype;
begin
    strm := TStream.Create;
    strm.Position := 0;
    strm.Free;

    jsonObj := TJSONObject.Create;
    valType := jsonObj.JSONType;
    jsonData := jsonObj.Find('key');
    jsonObj.Free;

    jsonArr := TJSONArray.Create;
    jsonArr.Free;

    parser := TJSONParser.Create('{"test": 1}');
    jsonData := parser.Parse;
    parser.Free;
end;

end.
