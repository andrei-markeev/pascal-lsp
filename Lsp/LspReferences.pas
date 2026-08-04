unit LspReferences;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser, contnrs,
  ParserContext, Token, Identifier, Symbols, ReservedWord, ProgramFile, UnitFile, TypeDefs, Scopes,
  LspUtils, LspState, LspConfig;

procedure HandleReferences(WriteStream: TStream; Id: TJSONData; Params: TJSONData);

implementation

function NormalizePath(const Path: string): string;
begin
  Result := ExpandFileName(Path);
  {$IFDEF WINDOWS}
  Result := LowerCase(Result);
  {$ENDIF}
end;

procedure CollectWorkspaceFiles(Files: TStringList; const CurrentUri: string);
  procedure ScanDir(const Dir: string; Depth: integer);
  var
    SR: TSearchRec;
    NormDir, Ext, DirName, FullPath: string;
  begin
    if Depth > 10 then exit;
    NormDir := IncludeTrailingPathDelimiter(Dir);
    if FindFirst(NormDir + '*', faAnyFile, SR) = 0 then
    begin
      try
        repeat
          if (SR.Name = '.') or (SR.Name = '..') then continue;
          if (SR.Attr and faDirectory) <> 0 then
          begin
            DirName := LowerCase(SR.Name);
            if (DirName <> '.git') and (DirName <> '.svn') and 
               (DirName <> 'node_modules') and (DirName <> 'backup') and 
               (DirName <> 'lib') and (DirName <> 'bin') and 
               (DirName <> 'out') and (DirName <> 'obj') and 
               (DirName <> 'temp') and (DirName <> 'tmp') then
              ScanDir(NormDir + SR.Name, Depth + 1);
          end
          else
          begin
            Ext := LowerCase(ExtractFileExt(SR.Name));
            if (Ext = '.pas') or (Ext = '.pp') or (Ext = '.inc') or 
               (Ext = '.lpr') or (Ext = '.dpr') then
            begin
              FullPath := NormalizePath(NormDir + SR.Name);
              if Files.IndexOf(FullPath) < 0 then
                Files.Add(FullPath);
            end;
          end;
        until FindNext(SR) <> 0;
      finally
        FindClose(SR);
      end;
    end;
  end;

var
  RootDir: string;
begin
  if (GConfig <> nil) and (GConfig.WorkspaceRoot <> '') and DirectoryExists(GConfig.WorkspaceRoot) then
    ScanDir(GConfig.WorkspaceRoot, 1);

  if Files.Count = 0 then
  begin
    RootDir := ExtractFilePath(UriToFilename(CurrentUri));
    if (RootDir <> '') and DirectoryExists(RootDir) then
      ScanDir(RootDir, 1);
  end;
end;

function ScreenAndReadFile(const FilePath, SearchName, OriginUnitName: string; var Content: string): boolean;
var
  F: File;
  Size, ReadBytes: integer;
  Buffer: string;
  LowerBuffer, LowerSearch, LowerUnit: string;
begin
  Result := false;
  Content := '';
  if not FileExists(FilePath) then exit;

  Assign(F, FilePath);
  {$I-}
  Reset(F, 1);
  {$I+}
  if IOResult <> 0 then exit;

  try
    Size := FileSize(F);
    if Size = 0 then exit;

    ReadBytes := Size;
    if ReadBytes > 65536 then ReadBytes := 65536;
    SetLength(Buffer, ReadBytes);
    BlockRead(F, Buffer[1], ReadBytes);

    LowerBuffer := LowerCase(Buffer);

    if (OriginUnitName <> '') and not SameText(ChangeFileExt(ExtractFileName(FilePath), ''), OriginUnitName) then
    begin
      LowerUnit := LowerCase(OriginUnitName);
      if Pos(LowerUnit, LowerBuffer) = 0 then
        exit(false);
    end;

    LowerSearch := LowerCase(SearchName);

    if Size <= 65536 then
    begin
      if Pos(LowerSearch, LowerBuffer) > 0 then
      begin
        Content := Buffer;
        Result := true;
      end;
    end
    else
    begin
      SetLength(Content, Size);
      Move(Buffer[1], Content[1], ReadBytes);
      BlockRead(F, Content[ReadBytes + 1], Size - ReadBytes);

      if Pos(LowerSearch, LowerCase(Content)) > 0 then
        Result := true;
    end;
  finally
    Close(F);
  end;
end;

procedure ScanFileForReferences(const FilePath, Content: string; 
  const TargetDeclFile: string; TargetDeclLine, TargetDeclPos: integer;
  IncludeDecl: boolean; SeenKeys: TStringList; var ResultJson: string);
var
  SavedScopes: array of TScope;
  SavedTypes: TFPHashList;
  SavedLoadedUnits: TFPHashList;
  TempCtx: TParserContext;
  fileToken: TToken;
  i: integer;
  curTok: TToken;
  curIdent: TIdentifier;
  refSym: TSymbol;
  declIdent: TIdentifier;
  declCtx: TParserContext;
  RefUri, Key: string;
begin
  SavedScopes := ScopesList;
  SavedTypes := TypeDefs.TypesList;
  SavedLoadedUnits := ParserContext.LoadedUnits;

  SetLength(ScopesList, 1);
  ScopesList[0] := TScope.Create;
  TypeDefs.TypesList := TFPHashList.Create;
  ParserContext.LoadedUnits := TFPHashList.Create;

  try
    TempCtx := TParserContext.Create(FilePath, Content);
    TempCtx.isDependency := false;

    try
      if PeekReservedWord(TempCtx, rwUnit) then
        fileToken := TUnitFile.Create(TempCtx)
      else
        fileToken := TProgramFile.Create(TempCtx);

      for i := 0 to TempCtx.tokensLen - 1 do
      begin
        curTok := TempCtx.Tokens[i];
        if (curTok <> nil) and (curTok is TIdentifier) then
        begin
          curIdent := TIdentifier(curTok);
          if curIdent.symbol <> nil then
          begin
            refSym := TSymbol(curIdent.symbol);
            if refSym <> nil then
            begin
              declIdent := refSym.declaration;
              if declIdent <> nil then
              begin
                declCtx := FindContextForCursor(declIdent.start);
                if (declCtx <> nil) and 
                   (NormalizePath(declCtx.filePath) = NormalizePath(TargetDeclFile)) and
                   (declIdent.line = TargetDeclLine) and
                   (declIdent.position = TargetDeclPos) then
                begin
                  if not IncludeDecl and ((curIdent = refSym.declaration) or (curIdent = refSym.implementationDecl)) then
                    continue;

                  RefUri := FilenameToUri(FilePath);
                  Key := RefUri + ':' + IntToStr(curIdent.line) + ':' + IntToStr(curIdent.position);
                  if SeenKeys.IndexOf(Key) < 0 then
                  begin
                    SeenKeys.Add(Key);
                    if ResultJson <> '[' then
                      ResultJson := ResultJson + ',';
                    ResultJson := ResultJson + '{' +
                      '"uri":"' + string(StringToJSONString(RefUri)) + '",' +
                      '"range":{' +
                        '"start":{"line":' + IntToStr(curIdent.line) + ',"character":' + IntToStr(curIdent.position) + '},' +
                        '"end":{"line":' + IntToStr(curIdent.line) + ',"character":' + IntToStr(curIdent.position + curIdent.len) + '}' +
                      '}' +
                    '}';
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    finally
      TempCtx.Free;
    end;
  finally
    for i := 0 to High(ScopesList) do
      if ScopesList[i] <> nil then ScopesList[i].Free;
    SetLength(ScopesList, 0);

    ClearLoadedUnits;
    ParserContext.LoadedUnits.Free;
    TypeDefs.TypesList.Free;

    ScopesList := SavedScopes;
    TypeDefs.TypesList := SavedTypes;
    ParserContext.LoadedUnits := SavedLoadedUnits;
  end;
end;

procedure HandleReferences(WriteStream: TStream; Id: TJSONData; Params: TJSONData);
var
  Uri, ResultJson, Response: string;
  TargetLine, TargetCharacter: integer;
  IncludeDecl: boolean;
  TargetIdent, Ident: TIdentifier;
  i, j: integer;
  curToken: TToken;
  Sym: TSymbol;
  ContextNode: TJSONData;
  SeenKeys, CandidateFiles: TStringList;
  TargetDeclIdent: TIdentifier;
  TargetDeclCtx: TParserContext;
  TargetDeclFile, SearchName, OriginUnitName: string;
  TargetDeclLine, TargetDeclPos: integer;
  CurrentFile, CandidatePath, FileContent: string;

  procedure TryAddRef(AIdent: TIdentifier; IsDeclaration: boolean);
  var
    RefCtxLoc: TParserContext;
    Key, RefUriLoc: string;
  begin
    if AIdent = nil then exit;
    if not IncludeDecl and IsDeclaration then exit;

    RefCtxLoc := FindContextForCursor(AIdent.start);
    if RefCtxLoc = nil then exit;

    RefUriLoc := FilenameToUri(RefCtxLoc.filePath);
    Key := RefUriLoc + ':' + IntToStr(AIdent.line) + ':' + IntToStr(AIdent.position);

    if SeenKeys.IndexOf(Key) >= 0 then exit;
    SeenKeys.Add(Key);

    if ResultJson <> '[' then
      ResultJson := ResultJson + ',';

    ResultJson := ResultJson + '{' +
      '"uri":"' + string(StringToJSONString(RefUriLoc)) + '",' +
      '"range":{' +
        '"start":{"line":' + IntToStr(AIdent.line) + ',"character":' + IntToStr(AIdent.position) + '},' +
        '"end":{"line":' + IntToStr(AIdent.line) + ',"character":' + IntToStr(AIdent.position + AIdent.len) + '}' +
      '}' +
    '}';
  end;

begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';

  ResultJson := '[';
  SeenKeys := TStringList.Create;
  SeenKeys.Sorted := true;
  SeenKeys.Duplicates := dupIgnore;

  try
    if Params <> nil then
    begin
      Uri := Params.FindPath('textDocument.uri').AsString;
      TargetLine := Params.FindPath('position.line').AsInteger;
      TargetCharacter := Params.FindPath('position.character').AsInteger;

      IncludeDecl := true;
      ContextNode := Params.FindPath('context.includeDeclaration');
      if ContextNode <> nil then
        IncludeDecl := ContextNode.AsBoolean;

      EnsureParsed(WriteStream, Uri);

      if (LastParserContext <> nil) and (LastParsedUri = Uri) then
      begin
        TargetIdent := nil;
        for i := 0 to LastParserContext.tokensLen - 1 do
        begin
          curToken := LastParserContext.Tokens[i];
          if (curToken <> nil) and (curToken is TIdentifier) then
          begin
            if (curToken.line = TargetLine) and
               (TargetCharacter >= curToken.position) and
               (TargetCharacter <= curToken.position + curToken.len) then
            begin
              TargetIdent := TIdentifier(curToken);
              Break;
            end;
          end;
        end;

        if TargetIdent <> nil then
        begin
          if TargetIdent.symbol <> nil then
            Sym := TSymbol(TargetIdent.symbol)
          else
            Sym := FindSymbol(TargetIdent.GetStr(), TargetIdent.start);

          if Sym <> nil then
          begin
            if IncludeDecl then
            begin
              if Sym.declaration <> nil then
                TryAddRef(Sym.declaration, true);
              if (Sym.implementationDecl <> nil) and (Sym.implementationDecl <> Sym.declaration) then
                TryAddRef(Sym.implementationDecl, true);
            end;

            for j := 0 to High(Sym.references) do
            begin
              Ident := Sym.references[j];
              TryAddRef(Ident, (Ident = Sym.declaration) or (Ident = Sym.implementationDecl));
            end;

            // Pass-through workspace search for references in implementation blocks / workspace units
            TargetDeclIdent := Sym.declaration;
            if TargetDeclIdent <> nil then
            begin
              TargetDeclCtx := FindContextForCursor(TargetDeclIdent.start);
              if TargetDeclCtx <> nil then
              begin
                TargetDeclFile := TargetDeclCtx.filePath;
                TargetDeclLine := TargetDeclIdent.line;
                TargetDeclPos := TargetDeclIdent.position;
                SearchName := TargetIdent.GetStr();
                OriginUnitName := ChangeFileExt(ExtractFileName(TargetDeclFile), '');

                CandidateFiles := TStringList.Create;
                CandidateFiles.CaseSensitive := false;
                try
                  CollectWorkspaceFiles(CandidateFiles, Uri);
                  CurrentFile := NormalizePath(UriToFilename(Uri));

                  for i := 0 to CandidateFiles.Count - 1 do
                  begin
                    CandidatePath := CandidateFiles[i];
                    if SameText(CandidatePath, CurrentFile) then continue;

                    if ScreenAndReadFile(CandidatePath, SearchName, OriginUnitName, FileContent) then
                    begin
                      ScanFileForReferences(CandidatePath, FileContent,
                        TargetDeclFile, TargetDeclLine, TargetDeclPos,
                        IncludeDecl, SeenKeys, ResultJson);
                    end;
                  end;
                finally
                  CandidateFiles.Free;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  finally
    SeenKeys.Free;
  end;

  ResultJson := ResultJson + ']';

  Response := Response + '"result":' + ResultJson + '}';
  SendResponse(WriteStream, Response);
end;

end.
