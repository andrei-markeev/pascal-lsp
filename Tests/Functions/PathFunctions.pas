program PathFunctions;

{$mode objfpc}
{$longstrings on}

uses sysutils;

var
    p, s, ext, fp, exp: string;
    c1, c2, c3, c4, c5, c6: char;
begin
    s := 'C' + DriveDelim + DriveSeparator + PathDelim + 'path' + DirectorySeparator + 'to' + PathSep + PathSeparator + 'file.txt';
    p := ExcludeTrailingPathDelimiter('C:\path\to\');
    p := IncludeTrailingPathDelimiter('C:\path\to');
    ext := ExtractFileExt(s);
    fp := ExtractFilePath(s);
    exp := ExpandFileName('relative/path.txt');
    c1 := PathDelim;
    c2 := DirectorySeparator;
    c3 := DriveDelim;
    c4 := DriveSeparator;
    c5 := PathSep;
    c6 := PathSeparator;
end.
