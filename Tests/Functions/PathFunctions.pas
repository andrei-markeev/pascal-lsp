program PathFunctions;

{$mode objfpc}
{$longstrings on}

uses sysutils;

var
    p, s, ext, fp, exp: string;
begin
    s := 'C:\path\to\file.txt';
    p := ExcludeTrailingPathDelimiter('C:\path\to\');
    p := IncludeTrailingPathDelimiter('C:\path\to');
    ext := ExtractFileExt(s);
    fp := ExtractFilePath(s);
    exp := ExpandFileName('relative/path.txt');
end.
