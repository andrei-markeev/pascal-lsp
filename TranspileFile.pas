program TranspileFile;

{$mode objfpc}
{$longstrings on}

uses
    sysutils, classes, CompilationMode, ParserContext, Token, Identifier, Symbols, Scopes, ReservedWord, TypeDecl, VarDecl,
    ParameterDecl, TypeDef, TypeDefs, ProgramFile, UnitFile, LspConfig, PointerSpec, RecordSpec, TypeSection,
    FunctionDecl, FunctionImpl, VarRef, Call, TypeSpec, RecordTypeDef, TranspileRegister;

procedure Transpile(fileName: string);
var
    i, j, k: integer;
    len: integer;
    f: file;
    fres: Text;
    contents: string;
    ctx: TParserContext;
    cur, tok, sectionTok: TToken;
    skips: array of integer;
    inserts: array of string;
    outFileName, outDir, relPath: string;
    rwEndTok: TReservedWord;
    typeDecl, firstDecl: TTypeDecl;
    varRef: TVarRef;
    sym, fieldSym, tSym: TSymbol;
    charPos, pLen, endPos, startPos, totalDecls, secDecls: integer;
    p, pEnd: PChar;
    recText: string;
    receiverText: string;
    procName: string;
    receiverStart: PChar;
    receiverLen: integer;
    selfTypeName: string;
    nameIdent: TIdentifier;
    diagKind, diagMsg: string;
begin
    ResetScopes;
    TypesList.Clear;
    ClearLoadedUnits;
    ResetTranspileRegister;
    WriteLn('Transpiling: ', fileName);
    Assign(f, fileName);
    Reset(f, 1);
    len := FileSize(f);
    SetLength(contents, len);
    if len > 0 then
        BlockRead(f, contents[1], len);
    Close(f);

    ctx := TParserContext.Create(fileName, contents);
    ctx.mode := cmUniversalPascal;

    if PeekReservedWord(ctx, rwUnit) then
        TUnitFile.Create(ctx)
    else
        TProgramFile.Create(ctx);

    for i := 0 to ctx.tokensLen - 1 do
    begin
        cur := ctx.Tokens[i];
        if (cur <> nil) and (cur.state in [tsError, tsMissing, tsWarning, tsSkipped]) then
        begin
            if cur.state = tsWarning then
                diagKind := 'Warning: '
            else
                diagKind := 'Error: ';

            if (cur.state = tsError) or (cur.state = tsWarning) then
            begin
                if cur.errorMessage <> '' then
                    diagMsg := cur.errorMessage
                else
                    diagMsg := 'Syntax error in ' + cur.tokenName;
            end
            else if cur.state = tsMissing then
            begin
                if cur.GetStr() <> '' then
                    diagMsg := 'Missing ''' + cur.GetStr() + ''''
                else
                    diagMsg := 'Missing ' + cur.tokenName;
            end
            else
            begin
                if cur.errorMessage <> '' then
                    diagMsg := cur.errorMessage
                else if cur.GetStr() <> '' then
                    diagMsg := 'Unexpected ''' + cur.GetStr() + ''''
                else
                    diagMsg := 'Unexpected ' + cur.tokenName;
            end;

            WriteLn(fileName, '(', cur.line + 1, ',', cur.position + 1, ') ', diagKind, diagMsg);
        end;
    end;

    SetLength(skips, len + 1);
    SetLength(inserts, len + 1);
    for i := 0 to len do
    begin
        skips[i] := 0;
        inserts[i] := '';
    end;

    // Skip {$mode universalpascal} directives
    p := PChar(contents);
    pEnd := p + len;
    while p < pEnd do
    begin
        if (p[0] = '{') and (p + 1 < pEnd) and (p[1] = '$') then
        begin
            startPos := p - PChar(contents);
            j := 2;
            while (p + j < pEnd) and (p[j] in [' ', #9]) do inc(j);
            if (p + j + 4 <= pEnd) and (strlicomp(p + j, 'mode', 4) = 0) then
            begin
                inc(j, 4);
                if (p + j < pEnd) and (p[j] in [' ', #9]) then
                begin
                    while (p + j < pEnd) and (p[j] in [' ', #9]) do inc(j);
                    if (p + j + 15 <= pEnd) and (strlicomp(p + j, 'universalpascal', 15) = 0) then
                    begin
                        inc(j, 15);
                        if (p + j < pEnd) and (p[j] in [' ', #9, '}']) then
                        begin
                            while (p + j < pEnd) and (p[j] in [' ', #9]) do inc(j);
                            if (p + j < pEnd) and (p[j] = '}') then
                            begin
                                inc(j);
                                while (p + j < pEnd) and (p[j] in [' ', #9]) do inc(j);
                                if (p + j + 1 < pEnd) and (p[j] = #13) and (p[j + 1] = #10) then
                                    inc(j, 2)
                                else if (p + j < pEnd) and (p[j] in [#10, #13]) then
                                    inc(j);

                                skips[startPos] := j;
                                inc(p, j);
                                continue;
                            end;
                        end;
                    end;
                end;
            end;
        end;
        inc(p);
    end;

    // implicitly dereferenced pointers e.g. "p.field" or "p[123]" become explicit: "p^.field" and "p^[123]"
    for i := 0 to ImplicitDerefsCount - 1 do
    begin
        charPos := ImplicitDerefs[i].pos - PChar(contents);
        if (charPos >= 0) and (charPos <= len) then
            inserts[charPos] := StringOfChar('^', ImplicitDerefs[i].count) + inserts[charPos];
    end;

    for i := 0 to ctx.tokensLen - 1 do
    begin
        cur := ctx.Tokens[i];
        if cur = nil then continue;
        charPos := cur.start - PChar(contents);
        if (charPos < 0) or (charPos > len) then continue;

        // "pointer to" Reserved Word => becomes "^"
        if (cur is TReservedWord) and (TReservedWord(cur).kind = rwPointer) then
        begin
            // check if followed by 'to'
            p := cur.start + cur.len;
            while (p < PChar(contents) + len) and (p[0] in [' ', #9, #13, #10]) do inc(p);
            if (p < PChar(contents) + len) and (strlicomp(p, 'to', 2) = 0) and ctx.IsSeparator(p[2]) then
            begin
                inc(p, 2);
                inserts[charPos] := inserts[charPos] + '^';
                skips[charPos] := p - cur.start;
            end;
        end;

        // 'optional' reserved word - simply drop it
        if (cur is TReservedWord) and (TReservedWord(cur).kind = rwOptional) then
        begin
            pLen := cur.len;
            p := cur.start + pLen;
            while (p < PChar(contents) + len) and (p[0] in [' ', #9]) do
            begin
                inc(pLen);
                inc(p);
            end;
            skips[charPos] := pLen;
        end;

        // partial records
        if cur is TTypeDecl then
        begin
            typeDecl := TTypeDecl(cur);
            sym := FindSymbol(typeDecl.ident.GetStr(), typeDecl.start);
            if (sym <> nil) and (sym.typeDef <> nil) and (sym.typeDef.kind = tkRecord) and TRecordTypeDef(sym.typeDef).isPartial then
            begin
                if (sym.declaration <> nil) and (sym.declaration <> typeDecl.ident) then
                begin
                    // Reference / secondary declaration -> skip entire TypeDecl (and TTypeSection if all decls in it are secondary partial records)
                    sectionTok := nil;
                    for j := 0 to ctx.tokensLen - 1 do
                    begin
                        if (ctx.Tokens[j] is TTypeSection) and
                           (ctx.Tokens[j].start <= typeDecl.start) and
                           (ctx.Tokens[j].start + ctx.Tokens[j].len >= typeDecl.start + typeDecl.len) then
                        begin
                            sectionTok := ctx.Tokens[j];
                            break;
                        end;
                    end;

                    totalDecls := 0;
                    secDecls := 0;
                    firstDecl := nil;
                    if sectionTok <> nil then
                    begin
                        for j := 0 to ctx.tokensLen - 1 do
                        begin
                            if (ctx.Tokens[j] is TTypeDecl) and
                               (ctx.Tokens[j].start >= sectionTok.start) and
                               (ctx.Tokens[j].start < sectionTok.start + sectionTok.len) then
                            begin
                                inc(totalDecls);
                                if firstDecl = nil then
                                    firstDecl := TTypeDecl(ctx.Tokens[j]);

                                tSym := FindSymbol(TTypeDecl(ctx.Tokens[j]).ident.GetStr(), ctx.Tokens[j].start);
                                if (tSym <> nil) and (tSym.typeDef <> nil) and (tSym.typeDef.kind = tkRecord) and
                                   TRecordTypeDef(tSym.typeDef).isPartial and (tSym.declaration <> nil) and
                                   (tSym.declaration <> TTypeDecl(ctx.Tokens[j]).ident) then
                                begin
                                    inc(secDecls);
                                end;
                            end;
                        end;
                    end;

                    if (totalDecls > 0) and (totalDecls = secDecls) and (typeDecl = firstDecl) then
                        startPos := sectionTok.start - PChar(contents)
                    else
                        startPos := typeDecl.start - PChar(contents);

                    pEnd := typeDecl.start + typeDecl.len;
                    while (pEnd < PChar(contents) + len) and (pEnd[0] in [' ', #9, #13, #10, ';']) do inc(pEnd);
                    skips[startPos] := pEnd - (PChar(contents) + startPos);
                end
                else
                begin
                    // Primary declaration -> drop 'partial' and emit full record
                    // Find partial RW in this TypeDecl
                    for j := 0 to ctx.tokensLen - 1 do
                    begin
                        tok := ctx.Tokens[j];
                        if (tok is TReservedWord) and (TReservedWord(tok).kind = rwPartial) and
                           (tok.start >= typeDecl.start) and (tok.start < typeDecl.start + typeDecl.len) then
                        begin
                            pLen := tok.len;
                            p := tok.start + pLen;
                            while (p < PChar(contents) + len) and (p[0] in [' ', #9]) do
                            begin
                                inc(pLen);
                                inc(p);
                            end;
                            skips[tok.start - PChar(contents)] := pLen;
                            break;
                        end;
                    end;

                    // Emit full record members from secondary partial declarations
                    // Find the 'end' token of this record spec
                    rwEndTok := nil;
                    for j := 0 to ctx.tokensLen - 1 do
                    begin
                        tok := ctx.Tokens[j];
                        if (tok is TReservedWord) and (TReservedWord(tok).kind = rwEnd) and
                           (tok.start >= typeDecl.start) and (tok.start <= typeDecl.start + typeDecl.len) then
                        begin
                            rwEndTok := TReservedWord(tok);
                            break;
                        end;
                    end;

                    if rwEndTok <> nil then
                    begin
                        recText := '';
                        for k := 0 to length(sym.children) - 1 do
                        begin
                            fieldSym := sym.children[k];
                            if fieldSym.kind in [skProcedure, skFunction, skConstructor, skDestructor] then
                                continue;

                            if (fieldSym.rangeToken <> nil) and
                               ((fieldSym.rangeToken.start < typeDecl.start) or (fieldSym.rangeToken.start > typeDecl.start + typeDecl.len)) then
                            begin
                                // Copy member declaration text from secondary declaration including leading indentation and trailing newline
                                p := fieldSym.rangeToken.start;
                                while (p > PChar(contents)) and ((p - 1)[0] in [' ', #9]) do dec(p);

                                pEnd := fieldSym.rangeToken.start + fieldSym.rangeToken.len;
                                while (pEnd < PChar(contents) + len) and (pEnd[0] in [';']) do inc(pEnd);
                                if (pEnd < PChar(contents) + len) and (pEnd[0] = #13) then inc(pEnd);
                                if (pEnd < PChar(contents) + len) and (pEnd[0] = #10) then inc(pEnd);

                                SetString(receiverText, p, pEnd - p);
                                if (length(receiverText) > 0) and not (receiverText[length(receiverText)] in [#10, #13]) then
                                    receiverText := receiverText + #13#10;

                                // Check if already added to recText to avoid duplicate field prints
                                if Pos(receiverText, recText) = 0 then
                                    recText := recText + receiverText;
                            end;
                        end;
                        if length(recText) > 0 then
                        begin
                            p := rwEndTok.start;
                            while (p > PChar(contents)) and ((p - 1)[0] in [' ', #9]) do dec(p);
                            endPos := p - PChar(contents);
                            inserts[endPos] := inserts[endPos] + recText;
                        end;
                    end;
                end;
            end;
        end;

        // Oberon method syntax declaration/implementation
        // procedure (var self: TMyType) DoStuff; => procedure TMyType_DoStuff(var self: TMyType);
        if ((cur is TFunctionDecl) and TFunctionDecl(cur).isOberonMethod) or
           ((cur is TFunctionImpl) and TFunctionImpl(cur).isOberonMethod) then
        begin
            if GetOberonReceiver(cur, receiverStart, receiverLen, selfTypeName) and
               (receiverStart <> nil) and (selfTypeName <> '') then
            begin
                if cur is TFunctionDecl then
                    nameIdent := TFunctionDecl(cur).nameIdent
                else
                    nameIdent := TFunctionImpl(cur).nameIdent;

                // Skip (var self: TMyType) after procedure/function
                pLen := receiverLen;
                p := receiverStart + pLen;
                while (p < PChar(contents) + len) and (p[0] in [' ', #9]) do
                begin
                    inc(pLen);
                    inc(p);
                end;
                skips[receiverStart - PChar(contents)] := pLen;

                // Prepend TMyType_ before procedure name
                startPos := nameIdent.start - PChar(contents);
                inserts[startPos] := selfTypeName + '_' + inserts[startPos];

                // Check if parameter list exists after procedure name
                p := nameIdent.start + nameIdent.len;
                while (p < PChar(contents) + len) and (p[0] in [' ', #9]) do inc(p);

                // Receiver param text without outer parens: e.g. "var self: TMyType"
                SetString(receiverText, receiverStart + 1, receiverLen - 2);

                if p[0] = '(' then
                begin
                    // Parameter list already exists -> inject "var self: TMyType; " right after '('
                    startPos := (p + 1) - PChar(contents);
                    inserts[startPos] := receiverText + '; ' + inserts[startPos];
                end
                else
                begin
                    // No parameter list -> inject "(var self: TMyType)" after nameIdent
                    startPos := (nameIdent.start + nameIdent.len) - PChar(contents);
                    inserts[startPos] := '(' + receiverText + ')' + inserts[startPos];
                end;
            end;
        end;

        // Oberon call sites x.DoStuff => TMyType_DoStuff(x)
        if cur is TVarRef then
        begin
            varRef := TVarRef(cur);
            if (varRef.symbol <> nil) and (varRef.symbol.kind in [skProcedure, skFunction]) then
            begin
                procName := '';
                if (varRef.symbol.parent <> nil) and (varRef.symbol.parent.kind = skTypeName) and
                   (varRef.symbol.rangeToken <> nil) and
                   (((varRef.symbol.rangeToken is TFunctionDecl) and TFunctionDecl(varRef.symbol.rangeToken).isOberonMethod) or
                    ((varRef.symbol.rangeToken is TFunctionImpl) and TFunctionImpl(varRef.symbol.rangeToken).isOberonMethod)) then
                    procName := varRef.symbol.parent.displayName + '_' + varRef.symbol.displayName
                else if Pos('_', varRef.symbol.displayName) > 0 then
                    procName := varRef.symbol.displayName;

                if procName <> '' then
                begin
                    // check if varRef was called via dot syntax x.DoStuff
                    p := varRef.start;
                    // find '.' before method identifier
                    pEnd := nil;
                    while (p < PChar(contents) + len) and (p < varRef.start + varRef.len) do
                    begin
                        if p[0] = '.' then
                        begin
                            pEnd := p;
                            break;
                        end;
                        inc(p);
                    end;

                    if pEnd <> nil then
                    begin
                        // Format: receiver.DoStuff
                        SetString(receiverText, varRef.start, pEnd - varRef.start); // "x"

                        // Find end of method identifier
                        p := pEnd + 1; // start of method name
                        while (p < PChar(contents) + len) and (p[0] in ['A'..'Z','a'..'z','0'..'9','_']) do inc(p);

                        // Replace "x.DoStuff" with "TMyType_DoStuff"
                        startPos := varRef.start - PChar(contents);
                        skips[startPos] := p - varRef.start;
                        inserts[startPos] := procName + inserts[startPos];

                        while (p < PChar(contents) + len) and (p[0] in [' ', #9]) do inc(p);

                        if p[0] = '(' then
                        begin
                            // Has args: x.DoStuff(a, b) -> TMyType_DoStuff(x, a, b)
                            // Inject "x, " right after '('
                            endPos := (p + 1) - PChar(contents);
                            inserts[endPos] := receiverText + ', ' + inserts[endPos];
                        end
                        else
                        begin
                            // No args: x.DoStuff -> TMyType_DoStuff(x)
                            // Inject "(x)" after method name
                            endPos := p - PChar(contents);
                            inserts[endPos] := '(' + receiverText + ')' + inserts[endPos];
                        end;
                    end;
                end;
            end;
        end;
    end;

    relPath := ExtractRelativePath(IncludeTrailingPathDelimiter(GetCurrentDir), ExpandFileName(fileName));
    outFileName := 'out' + PathDelim + relPath;
    outDir := ExtractFilePath(outFileName);
    if outDir <> '' then
        ForceDirectories(outDir);

    Assign(fres, outFileName);
    Rewrite(fres);

    i := 0;
    while i < len do
    begin
        if length(inserts[i]) > 0 then
            Write(fres, inserts[i]);

        if skips[i] > 0 then
            inc(i, skips[i])
        else
        begin
            Write(fres, contents[i + 1]);
            inc(i);
        end;
    end;

    if length(inserts[len]) > 0 then
        Write(fres, inserts[len]);

    Close(fres);
    WriteLn('Wrote transpiled file to: ', outFileName);

    ctx.Free;
    TypesList.Clear;
    ResetScopes;
    ClearLoadedUnits;
    ResetTranspileRegister;
end;

var
    i: integer;
begin
    if ParamCount = 0 then
    begin
        WriteLn('Usage: TranspileFile.exe <file1> ... <fileN>');
        exit;
    end;

    GConfig.SetWorkspaceRoot(GetCurrentDir);
    GConfig.ResolveSearchPaths;

    for i := 1 to ParamCount do
        Transpile(ParamStr(i));
end.
