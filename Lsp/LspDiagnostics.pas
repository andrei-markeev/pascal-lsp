unit LspDiagnostics;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Token, ReservedWord, ProgramFile, UnitFile,
  LspUtils, LspState;

procedure HandleFileChange(WriteStream: TStream; const Uri: string; const Content: string);
procedure HandleFileClose(const Uri: string);

implementation

procedure HandleFileChange(WriteStream: TStream; const Uri: string; const Content: string);
begin
  ParseDocument(WriteStream, Uri, Content, true);
end;

procedure HandleFileClose(const Uri: string);
begin
  GDocumentCache.Remove(Uri);
end;

end.
