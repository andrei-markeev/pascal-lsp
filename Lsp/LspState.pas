unit LspState;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser, contnrs,
  ParserContext, Token, ReservedWord, ProgramFile, UnitFile, TypeDefs, Scopes,
  LspConfig, LspUtils;

type
  TParsedDocument = class
  public
    Uri: string;
    Content: string;
    Context: TParserContext;
    FileToken: TToken;
    Scopes: array of TScope;
    TypesList: TFPHashList;
    LoadedUnits: TFPHashList;
    constructor Create(const AUri, AContent: string);
    destructor Destroy; override;
  end;

  TLruDocumentCache = class
  private
    FItems: TStringList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Put(doc: TParsedDocument);
    function Get(const AUri: string): TParsedDocument;
    function RestoreState(const AUri: string): boolean;
    procedure Remove(const AUri: string);
    procedure Clear;
  end;

var
  LastParsedUri: string = '';
  LastParserContext: TParserContext = nil;
  LastFileToken: TToken = nil;
  GDocumentCache: TLruDocumentCache = nil;

procedure FreeLastParsed;
procedure ParseDocument(WriteStream: TStream; const Uri: string; const Content: string; PublishDiag: boolean);
procedure EnsureParsed(WriteStream: TStream; const Uri: string);

implementation

constructor TParsedDocument.Create(const AUri, AContent: string);
begin
  Uri := AUri;
  Content := AContent;
  Context := nil;
  FileToken := nil;
  TypesList := TFPHashList.Create;
  LoadedUnits := TFPHashList.Create;
  SetLength(Scopes, 0);
end;

destructor TParsedDocument.Destroy;
var
  i: integer;
  ctx: TParserContext;
begin
  if (LastParserContext = Context) or (LastParsedUri = Uri) then
  begin
    LastParsedUri := '';
    LastParserContext := nil;
    LastFileToken := nil;
  end;

  if Context <> nil then
  begin
    Context.Free;
    Context := nil;
  end;
  FileToken := nil;

  for i := 0 to High(Scopes) do
    if Scopes[i] <> nil then
      Scopes[i].Free;
  SetLength(Scopes, 0);

  if TypesList <> nil then
  begin
    if TypeDefs.TypesList = TypesList then
      TypeDefs.TypesList := TFPHashList.Create;
    TypesList.Free;
    TypesList := nil;
  end;

  if LoadedUnits <> nil then
  begin
    if ParserContext.LoadedUnits = LoadedUnits then
      ParserContext.LoadedUnits := TFPHashList.Create;
    for i := 0 to LoadedUnits.Count - 1 do
    begin
      ctx := TParserContext(LoadedUnits.Items[i]);
      if (ctx <> nil) and ctx.isDependency then
        ctx.Free;
    end;
    LoadedUnits.Free;
    LoadedUnits := nil;
  end;

  inherited Destroy;
end;

constructor TLruDocumentCache.Create;
begin
  FItems := TStringList.Create;
  FItems.CaseSensitive := false;
end;

destructor TLruDocumentCache.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;

procedure TLruDocumentCache.Put(doc: TParsedDocument);
var
  idx: integer;
  oldDoc: TParsedDocument;
begin
  if doc = nil then exit;
  idx := FItems.IndexOf(doc.Uri);
  if idx >= 0 then
  begin
    oldDoc := TParsedDocument(FItems.Objects[idx]);
    if oldDoc <> doc then
      oldDoc.Free;
    FItems.Objects[idx] := doc;
    if idx > 0 then
      FItems.Move(idx, 0);
  end
  else
  begin
    FItems.InsertObject(0, doc.Uri, doc);
    while FItems.Count > GConfig.MaxDocumentCacheSize do
    begin
      idx := FItems.Count - 1;
      TParsedDocument(FItems.Objects[idx]).Free;
      FItems.Delete(idx);
    end;
  end;
end;

function TLruDocumentCache.Get(const AUri: string): TParsedDocument;
var
  idx: integer;
begin
  idx := FItems.IndexOf(AUri);
  if idx >= 0 then
  begin
    Result := TParsedDocument(FItems.Objects[idx]);
    if idx > 0 then
      FItems.Move(idx, 0);
  end
  else
    Result := nil;
end;

function TLruDocumentCache.RestoreState(const AUri: string): boolean;
var
  doc: TParsedDocument;
begin
  doc := Get(AUri);
  if doc <> nil then
  begin
    LastParsedUri := doc.Uri;
    LastParserContext := doc.Context;
    LastFileToken := doc.FileToken;
    ScopesList := doc.Scopes;
    TypeDefs.TypesList := doc.TypesList;
    ParserContext.LoadedUnits := doc.LoadedUnits;
    Result := true;
  end
  else
    Result := false;
end;

procedure TLruDocumentCache.Remove(const AUri: string);
var
  idx: integer;
begin
  idx := FItems.IndexOf(AUri);
  if idx >= 0 then
  begin
    TParsedDocument(FItems.Objects[idx]).Free;
    FItems.Delete(idx);
  end;
end;

procedure TLruDocumentCache.Clear;
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    TParsedDocument(FItems.Objects[i]).Free;
  FItems.Clear;
end;

procedure FreeLastParsed;
begin
  LastParsedUri := '';
  LastParserContext := nil;
  LastFileToken := nil;
end;

procedure ParseDocument(WriteStream: TStream; const Uri: string; const Content: string; PublishDiag: boolean);
var
  doc: TParsedDocument;
  ctx: TParserContext;
  fileToken: TToken;
  i: integer;
  cur: TToken;
  DiagnosticsJson: string;
  DiagCount: integer;
begin
  GDocumentCache.Remove(Uri);

  doc := TParsedDocument.Create(Uri, Content);
  TypeDefs.TypesList := doc.TypesList;
  ParserContext.LoadedUnits := doc.LoadedUnits;

  SetLength(ScopesList, 1);
  ScopesList[0] := TScope.Create;

  ctx := TParserContext.Create(UriToFilename(Uri), Content);
  if PeekReservedWord(ctx, rwUnit) then
    fileToken := TUnitFile.Create(ctx)
  else
    fileToken := TProgramFile.Create(ctx);

  doc.Context := ctx;
  doc.FileToken := fileToken;
  doc.Scopes := ScopesList;

  LastParsedUri := Uri;
  LastParserContext := ctx;
  LastFileToken := fileToken;

  GDocumentCache.Put(doc);

  if PublishDiag and (WriteStream <> nil) then
  begin
    DiagnosticsJson := '';
    DiagCount := 0;

    for i := 0 to ctx.tokensLen - 1 do
    begin
      cur := ctx.Tokens[i];
      if cur.state in [tsError, tsMissing] then
      begin
        if DiagCount > 0 then
          DiagnosticsJson := DiagnosticsJson + ',';

        DiagnosticsJson := DiagnosticsJson + '{' +
          '"range":{' +
            '"start":{"line":' + IntToStr(cur.line) + ',"character":' + IntToStr(cur.position) + '},' +
            '"end":{"line":' + IntToStr(cur.line) + ',"character":' + IntToStr(cur.position + cur.len) + '}' +
          '},' +
          '"severity":1,';

        if cur.state = tsError then
          DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString(cur.errorMessage)) + '"'
        else
          if cur.GetStr() <> '' then
            DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString('Missing ''' + cur.GetStr() + '''')) + '"'
          else
            DiagnosticsJson := DiagnosticsJson + '"message":' + '"' + string(StringToJSONString('Missing ' + cur.tokenName)) + '"';

        DiagnosticsJson := DiagnosticsJson + '}';
        inc(DiagCount);
      end;
    end;

    SendResponse(WriteStream, 
      '{"jsonrpc":"2.0","method":"textDocument/publishDiagnostics","params":{' +
        '"uri":' + '"' + string(StringToJSONString(Uri)) + '"' + ',' +
        '"diagnostics":[' + DiagnosticsJson + ']' +
      '}}'
    );
  end;
end;

procedure EnsureParsed(WriteStream: TStream; const Uri: string);
var
  Content: string;
  Filename: string;
  SL: TStringList;
begin
  if Uri = '' then exit;
  if (LastParsedUri = Uri) and (LastParserContext <> nil) then exit;

  if GDocumentCache.RestoreState(Uri) then exit;

  Filename := UriToFilename(Uri);
  if FileExists(Filename) then
  begin
    SL := TStringList.Create;
    try
      SL.LoadFromFile(Filename);
      Content := SL.Text;
    finally
      SL.Free;
    end;
    ParseDocument(WriteStream, Uri, Content, false);
  end;
end;

initialization
  GDocumentCache := TLruDocumentCache.Create;
finalization
  GDocumentCache.Free;
  GDocumentCache := nil;
end.

