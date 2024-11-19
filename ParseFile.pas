program ParseFile;

{$mode objfpc}
{$longstrings on}

uses
    sysutils, ParserContext, Token, Symbols, Scopes, ReservedWord, TypeDecl,
    ParameterDecl, TypeDefs, ProgramFile, UnitFile;

procedure Parse(fileName: string);
var
    i, j: integer;
    len: integer;
    f: file;
    fres: Text;
    contents: string;
    ctx: TParserContext;
    cur: TToken;
    fileToken: TToken;
    inserts: array of string;
begin
    WriteLn(fileName);
    Assign(f, fileName);
    Reset(f, 1);
    len := FileSize(f);
    SetLength(contents, len);
    BlockRead(f, contents[1], len);
    Close(f);

    ctx := TParserContext.Create(contents);

    if PeekReservedWord(ctx, rwUnit) then
        fileToken := TUnitFile.Create(ctx)
    else
        fileToken := TProgramFile.Create(ctx);

    SetLength(inserts, len + 1);
    for i := 0 to len do
        inserts[i] := '';

    for i := 0 to ctx.tokensLen - 1 do
    begin
        cur := ctx.Tokens[i];
        j := cur.start - PChar(contents);
        if j > len then continue;

        if cur.state = tsInvisible then
            continue;

        if cur.state = tsMissing then inserts[j] := inserts[j] + '<' + cur.tokenName + ' MISSING="' + cur.GetStr + '" />'
        else if cur.state = tsSkipped then inserts[j] := inserts[j] + '<' + cur.tokenName + ' SKIPPED="true">'
        else if cur.state = tsError then inserts[j] := inserts[j] + '<' + cur.tokenName + ' ERROR="' + cur.errorMessage + '">'
        else if cur.state = tsEndOf then inserts[j] := inserts[j] + '</' + cur.tokenName + '>'
        else inserts[j] := inserts[j] + '<' + cur.tokenName + '>';

        if cur.isPrimitive and (cur.state <> tsMissing) then
        begin
            j := cur.start - PChar(contents) + cur.len;
            if j > len then
            begin
                WriteLn('j=', j, ' i=',i, ' cur.tokenName=', cur.tokenName, ' cur.start=', cur.start - PChar(contents), ' cur.len=', cur.len, ' len=', len);
                j := len;
            end;
            inserts[j] := '</' + cur.tokenName + '>' + inserts[j];
        end;
    end;

    Assign(fres, StringReplace(fileName, '.pas', '.out.xml', []));
    Rewrite(fres);
    for i := 1 to len do
    begin
        if length(inserts[i - 1]) <> 0 then Write(fres, inserts[i - 1]);
        Write(fres, contents[i]);
    end;
    if length(inserts[len]) <> 0 then Write(fres, inserts[len]);
    Close(fres);

    fileToken.Free;
    TypesList.Clear;
    ResetScopes;

end;

var
    i: integer;
begin
    if paramCount = 0 then
    begin
        WriteLn('Usage: ParseFile.exe <file1> ... <fileN>');
        exit;
    end;

    for i := 1 to ParamCount do
        Parse(ParamStr(i));

end.
