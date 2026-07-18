unit LspUtils;

{$mode objfpc}
{$longstrings on}

interface

uses
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  sysutils, classes;

function GetStdinHandle: THandle;
function GetStdoutHandle: THandle;
function ReadLine(Stream: TStream; var Line: string): boolean;
procedure WriteStreamStr(Stream: TStream; const S: string);
procedure SendResponse(WriteStream: TStream; const Payload: string);
function UriToFilename(const Uri: string): string;
function FilenameToUri(const Filename: string): string;
function LineCharToOffset(const Content: string; TargetLine, TargetCharacter: integer): integer;

implementation

function GetStdinHandle: THandle;
begin
  {$IFDEF WINDOWS}
  Result := GetStdHandle(STD_INPUT_HANDLE);
  {$ELSE}
  Result := 0;
  {$ENDIF}
end;

function GetStdoutHandle: THandle;
begin
  {$IFDEF WINDOWS}
  Result := GetStdHandle(STD_OUTPUT_HANDLE);
  {$ELSE}
  Result := 1;
  {$ENDIF}
end;

function ReadLine(Stream: TStream; var Line: string): boolean;
var
  ch: char;
  bytesRead: integer;
begin
  Line := '';
  repeat
    bytesRead := Stream.Read(ch, 1);
    if bytesRead = 1 then
    begin
      if ch = #10 then
        exit(true);
      if ch <> #13 then
        Line := Line + ch;
    end
    else
      exit(false);
  until false;
end;

procedure WriteStreamStr(Stream: TStream; const S: string);
begin
  if S <> '' then
    Stream.Write(S[1], Length(S));
end;

procedure SendResponse(WriteStream: TStream; const Payload: string);
var
  Header: string;
begin
  Header := 'Content-Length: ' + IntToStr(Length(Payload)) + #13#10 +
            'Content-Type: application/vscode-jsonrpc; charset=utf-8' + #13#10#13#10;
  WriteStreamStr(WriteStream, Header);
  WriteStreamStr(WriteStream, Payload);
end;

function UriToFilename(const Uri: string): string;
begin
  if Copy(Uri, 1, 8) = 'file:///' then
    Result := Copy(Uri, 9, Length(Uri) - 8)
  else
    Result := Uri;
  Result := StringReplace(Result, '%20', ' ', [rfReplaceAll]);
  Result := StringReplace(Result, '%3A', ':', [rfReplaceAll]);
  Result := StringReplace(Result, '%3a', ':', [rfReplaceAll]);
end;

function FilenameToUri(const Filename: string): string;
var
  UnixPath: string;
begin
  UnixPath := StringReplace(Filename, '\', '/', [rfReplaceAll]);
  UnixPath := StringReplace(UnixPath, ' ', '%20', [rfReplaceAll]);
  if (Length(UnixPath) > 1) and (UnixPath[2] = ':') then
    Result := 'file:///' + UnixPath
  else
    Result := 'file://' + UnixPath;
end;

function LineCharToOffset(const Content: string; TargetLine, TargetCharacter: integer): integer;
var
  i, curLine, curChar, len: integer;
begin
  Result := 0;
  len := Length(Content);
  if len = 0 then exit;

  curLine := 0;
  curChar := 0;
  i := 1;
  while i <= len do
  begin
    if (curLine = TargetLine) and (curChar = TargetCharacter) then
    begin
      Result := i;
      exit;
    end;

    if (Content[i] = #13) and (i < len) and (Content[i+1] = #10) then
    begin
      if curLine = TargetLine then
      begin
        Result := i;
        exit;
      end;
      inc(i, 2);
      inc(curLine);
      curChar := 0;
    end
    else if Content[i] in [#10, #13] then
    begin
      if curLine = TargetLine then
      begin
        Result := i;
        exit;
      end;
      inc(i);
      inc(curLine);
      curChar := 0;
    end
    else
    begin
      inc(i);
      inc(curChar);
    end;
  end;

  if curLine = TargetLine then
    Result := len + 1;
end;

end.
