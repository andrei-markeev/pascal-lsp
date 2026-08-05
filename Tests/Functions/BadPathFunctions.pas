program BadPathFunctions;

{$mode objfpc}
{$longstrings on}

uses sysutils;

var
    p: string;
begin
    p := ExcludeTrailingPathDelimiter();
    p := IncludeTrailingPathDelimiter(123);
    p := ExtractFileExt('a', 'b');
    p := ExtractFilePath(;
    p := ExpandFileName;
    PathDelim := '/';
end.
