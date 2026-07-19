unit LspConfig;

{$mode objfpc}
{$longstrings on}

interface

uses
    classes, sysutils;

type
    TLspConfig = class
    public
        UseConfiguredPaths: boolean;
        ConfiguredPaths: TStringList;
        ReadLpi: boolean;
        ReadDproj: boolean;
        ScanProjectFolders: boolean;
        WorkspaceRoot: string;
        ResolvedSearchPaths: TStringList;

        MaxDocumentCacheSize: integer;

        constructor Create;
        destructor Destroy; override;
        procedure SetWorkspaceRoot(const ARoot: string);
        procedure ResolveSearchPaths;
        procedure Clear;
    private
        procedure LoadFromLpi(const APath: string);
        procedure LoadFromDproj(const APath: string);
        procedure ScanFolders(const APath: string; Depth: integer);
    end;

var
    GConfig: TLspConfig;

implementation

function IsAbsolute(const Path: string): boolean;
begin
    if Length(Path) = 0 then exit(false);
    {$IFDEF WINDOWS}
    Result := (Length(Path) >= 2) and (Path[2] = ':');
    {$ELSE}
    Result := Path[1] = '/';
    {$ENDIF}
end;

constructor TLspConfig.Create;
begin
    UseConfiguredPaths := false;
    ConfiguredPaths := TStringList.Create;
    ReadLpi := true;
    ReadDproj := true;
    ScanProjectFolders := true;
    MaxDocumentCacheSize := 10;
    WorkspaceRoot := '';
    ResolvedSearchPaths := TStringList.Create;
end;

destructor TLspConfig.Destroy;
begin
    ConfiguredPaths.Free;
    ResolvedSearchPaths.Free;
    inherited Destroy;
end;

procedure TLspConfig.Clear;
begin
    ResolvedSearchPaths.Clear;
end;

procedure TLspConfig.SetWorkspaceRoot(const ARoot: string);
begin
    WorkspaceRoot := ExcludeTrailingPathDelimiter(ARoot);
end;

procedure TLspConfig.ScanFolders(const APath: string; Depth: integer);
var
    SR: TSearchRec;
    DirName: string;
    HasPasFiles: boolean;
    SubDirs: TStringList;
    i: integer;
    NormalizedPath: string;
begin
    if Depth > 3 then Exit;
    
    HasPasFiles := false;
    SubDirs := TStringList.Create;
    NormalizedPath := IncludeTrailingPathDelimiter(APath);
    try
        if FindFirst(NormalizedPath + '*', faAnyFile, SR) = 0 then
        begin
            repeat
                if (SR.Name = '.') or (SR.Name = '..') then
                    continue;
                    
                if (SR.Attr and faDirectory) <> 0 then
                begin
                    DirName := LowerCase(SR.Name);
                    // Check ignore list
                    if (DirName <> '.git') and (DirName <> '.svn') and 
                       (DirName <> 'node_modules') and (DirName <> 'backup') and 
                       (DirName <> 'lib') and (DirName <> 'bin') and 
                       (DirName <> 'out') and (DirName <> 'obj') and 
                       (DirName <> 'temp') and (DirName <> 'tmp') then
                    begin
                        SubDirs.Add(NormalizedPath + SR.Name);
                    end;
                end
                else
                begin
                    // Check if it is a pascal source file
                    if (ExtractFileExt(LowerCase(SR.Name)) = '.pas') or 
                       (ExtractFileExt(LowerCase(SR.Name)) = '.pp') then
                    begin
                        HasPasFiles := true;
                    end;
                end;
            until FindNext(SR) <> 0;
            FindClose(SR);
        end;

        if HasPasFiles then
        begin
            if ResolvedSearchPaths.IndexOf(APath) = -1 then
                ResolvedSearchPaths.Add(APath);
        end;

        // Recurse into subdirectories
        for i := 0 to SubDirs.Count - 1 do
        begin
            ScanFolders(SubDirs[i], Depth + 1);
        end;
    finally
        SubDirs.Free;
    end;
end;

procedure TLspConfig.LoadFromLpi(const APath: string);
var
    F: TextFile;
    Line: string;
    P, P2: integer;
    PathsStr: string;
    PathsList: TStringList;
    ProjDir: string;
    i: integer;
    ResolvedPath: string;
begin
    if not FileExists(APath) then Exit;
    ProjDir := ExtractFilePath(APath);
    
    AssignFile(F, APath);
    Reset(F);
    try
        while not Eof(F) do
        begin
            ReadLn(F, Line);
            P := Pos('<OtherUnitFiles Value="', Line);
            if P > 0 then
            begin
                Inc(P, Length('<OtherUnitFiles Value="'));
                P2 := Pos('"', Copy(Line, P, Length(Line)));
                if P2 > 0 then
                begin
                    PathsStr := Copy(Line, P, P2 - 1);
                    PathsList := TStringList.Create;
                    try
                        PathsList.Delimiter := ';';
                        PathsList.StrictDelimiter := true;
                        PathsList.DelimitedText := PathsStr;
                        for i := 0 to PathsList.Count - 1 do
                        begin
                            if Pos('$', PathsList[i]) = 0 then
                            begin
                                if IsAbsolute(PathsList[i]) then
                                    ResolvedPath := ExcludeTrailingPathDelimiter(PathsList[i])
                                else
                                    ResolvedPath := ExcludeTrailingPathDelimiter(ExpandFileName(ProjDir + PathsList[i]));
                                    
                                if DirectoryExists(ResolvedPath) and (ResolvedSearchPaths.IndexOf(ResolvedPath) = -1) then
                                    ResolvedSearchPaths.Add(ResolvedPath);
                            end;
                        end;
                    finally
                        PathsList.Free;
                    end;
                end;
            end;
        end;
    finally
        CloseFile(F);
    end;
end;

procedure TLspConfig.LoadFromDproj(const APath: string);
var
    F: TextFile;
    Line: string;
    P, P2: integer;
    PathsStr: string;
    PathsList: TStringList;
    ProjDir: string;
    i: integer;
    ResolvedPath: string;
begin
    if not FileExists(APath) then Exit;
    ProjDir := ExtractFilePath(APath);
    
    AssignFile(F, APath);
    Reset(F);
    try
        while not Eof(F) do
        begin
            ReadLn(F, Line);
            P := Pos('<DCC_UnitSearchPath>', Line);
            if P > 0 then
            begin
                Inc(P, Length('<DCC_UnitSearchPath>'));
                P2 := Pos('</DCC_UnitSearchPath>', Line);
                if P2 > 0 then
                begin
                    PathsStr := Copy(Line, P, P2 - P);
                    PathsList := TStringList.Create;
                    try
                        PathsList.Delimiter := ';';
                        PathsList.StrictDelimiter := true;
                        PathsList.DelimitedText := PathsStr;
                        for i := 0 to PathsList.Count - 1 do
                        begin
                            if Pos('$', PathsList[i]) = 0 then
                            begin
                                if IsAbsolute(PathsList[i]) then
                                    ResolvedPath := ExcludeTrailingPathDelimiter(PathsList[i])
                                else
                                    ResolvedPath := ExcludeTrailingPathDelimiter(ExpandFileName(ProjDir + PathsList[i]));
                                    
                                if DirectoryExists(ResolvedPath) and (ResolvedSearchPaths.IndexOf(ResolvedPath) = -1) then
                                    ResolvedSearchPaths.Add(ResolvedPath);
                            end;
                        end;
                    finally
                        PathsList.Free;
                    end;
                end;
            end;
        end;
    finally
        CloseFile(F);
    end;
end;

procedure TLspConfig.ResolveSearchPaths;
var
    i: integer;
    ResolvedPath: string;
    SR: TSearchRec;
    NormalizedWorkspace: string;
begin
    Clear;
    if WorkspaceRoot = '' then Exit;
    
    // 1. Configured Paths
    if UseConfiguredPaths then
    begin
        for i := 0 to ConfiguredPaths.Count - 1 do
        begin
            if IsAbsolute(ConfiguredPaths[i]) then
                ResolvedPath := ExcludeTrailingPathDelimiter(ConfiguredPaths[i])
            else
                ResolvedPath := ExcludeTrailingPathDelimiter(ExpandFileName(WorkspaceRoot + PathDelim + ConfiguredPaths[i]));
                
            if DirectoryExists(ResolvedPath) and (ResolvedSearchPaths.IndexOf(ResolvedPath) = -1) then
                ResolvedSearchPaths.Add(ResolvedPath);
        end;
    end;
    
    NormalizedWorkspace := IncludeTrailingPathDelimiter(WorkspaceRoot);

    // 2. Project Files
    if FindFirst(NormalizedWorkspace + '*', faAnyFile, SR) = 0 then
    begin
        repeat
            if (SR.Attr and faDirectory) = 0 then
            begin
                if ReadLpi and (ExtractFileExt(LowerCase(SR.Name)) = '.lpi') then
                    LoadFromLpi(NormalizedWorkspace + SR.Name)
                else if ReadDproj and (ExtractFileExt(LowerCase(SR.Name)) = '.dproj') then
                    LoadFromDproj(NormalizedWorkspace + SR.Name);
            end;
        until FindNext(SR) <> 0;
        FindClose(SR);
    end;
    
    // 3. Scan Folders
    if ScanProjectFolders then
    begin
        ScanFolders(WorkspaceRoot, 1);
    end;
end;

initialization
    GConfig := TLspConfig.Create;
finalization
    GConfig.Free;
    GConfig := nil;
end.
