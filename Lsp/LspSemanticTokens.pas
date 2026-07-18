unit LspSemanticTokens;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Token, Identifier, Symbols, ReservedWord, TypeDefs, TypeDef,
  LspUtils, LspState;

type
  TSemanticToken = record
    line: integer;
    position: integer;
    len: integer;
    tokenType: integer;
    tokenModifiers: integer;
  end;
  TSemanticTokenArray = array of TSemanticToken;

procedure HandleSemanticTokens(WriteStream: TStream; const Uri: string; Id: TJSONData);

implementation

procedure AddSemanticToken(var tokens: TSemanticTokenArray; var count, capacity: integer; line, pos, len, tType, tModifiers: integer);
begin
  if count >= capacity then
  begin
    capacity := capacity + 256;
    SetLength(tokens, capacity);
  end;
  tokens[count].line := line;
  tokens[count].position := pos;
  tokens[count].len := len;
  tokens[count].tokenType := tType;
  tokens[count].tokenModifiers := tModifiers;
  inc(count);
end;

procedure ScanComments(const Content: string; var tokens: TSemanticTokenArray; var count, capacity: integer);
var
  p: PChar;
  lineStart: PChar;
  line, pos: integer;
  commentStart: PChar;
  commentLen: integer;
begin
  if Content = '' then exit;
  p := PChar(Content);
  lineStart := p;
  line := 0;
  while p[0] <> #0 do
  begin
    if (p[0] = #13) and (p[1] = #10) then
    begin
      inc(p, 2);
      inc(line);
      lineStart := p;
    end
    else if p[0] in [#10, #13] then
    begin
      inc(p);
      inc(line);
      lineStart := p;
    end
    else if (p[0] = '/') and (p[1] = '/') then
    begin
      commentStart := p;
      pos := p - lineStart;
      inc(p, 2);
      while not (p[0] in [#10, #13, #0]) do
        inc(p);
      commentLen := p - commentStart;
      AddSemanticToken(tokens, count, capacity, line, pos, commentLen, 13, 0); // 13 = comment
    end
    else if p[0] = '{' then
    begin
      commentStart := p;
      pos := p - lineStart;
      inc(p);
      while (p[0] <> '}') and (p[0] <> #0) do
      begin
        if (p[0] = #13) and (p[1] = #10) then
        begin
          inc(p, 2);
          inc(line);
          lineStart := p;
        end
        else if p[0] in [#10, #13] then
        begin
          inc(p);
          inc(line);
          lineStart := p;
        end
        else
          inc(p);
      end;
      if p[0] = '}' then
        inc(p);
      commentLen := p - commentStart;
      AddSemanticToken(tokens, count, capacity, line, pos, commentLen, 13, 0); // 13 = comment
    end
    else
      inc(p);
  end;
end;

procedure QuickSortSemanticTokens(var tokens: TSemanticTokenArray; Low, High: integer);
var
  i, j: integer;
  pivot: TSemanticToken;
  temp: TSemanticToken;
begin
  if Low >= High then exit;
  pivot := tokens[Low + (High - Low) div 2];
  i := Low;
  j := High;
  repeat
    while (tokens[i].line < pivot.line) or ((tokens[i].line = pivot.line) and (tokens[i].position < pivot.position)) do
      inc(i);
    while (tokens[j].line > pivot.line) or ((tokens[j].line = pivot.line) and (tokens[j].position > pivot.position)) do
      dec(j);
    if i <= j then
    begin
      temp := tokens[i];
      tokens[i] := tokens[j];
      tokens[j] := temp;
      inc(i);
      dec(j);
    end;
  until i > j;
  QuickSortSemanticTokens(tokens, Low, j);
  QuickSortSemanticTokens(tokens, i, High);
end;

procedure HandleSemanticTokens(WriteStream: TStream; const Uri: string; Id: TJSONData);
var
  Response: string;
  tokens: TSemanticTokenArray;
  count, capacity: integer;
  i, k: integer;
  curToken: TToken;
  ident: TIdentifier;
  sym: TSymbol;
  tokenType, tokenModifiers: integer;
  isParam: boolean;
  parentsStack: array of string;
  parentsCount: integer;
  dataJson: string;
  prevLine, prevChar: integer;
  deltaLine, deltaChar: integer;
  tName: shortstring;
  identName: shortstring;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';
    
  count := 0;
  capacity := 256;
  SetLength(tokens, capacity);
  
  if (LastParserContext <> nil) and (LastParsedUri = Uri) then
  begin
    ScanComments(LastParserContext.GetContents, tokens, count, capacity);
    
    parentsCount := 0;
    SetLength(parentsStack, 64);
    
    for k := 0 to LastParserContext.tokensLen - 1 do
    begin
      curToken := LastParserContext.Tokens[k];
      if curToken = nil then continue;
      
      if curToken.state = tsEndOf then
      begin
        if parentsCount > 0 then
          dec(parentsCount);
        continue;
      end;
      
      if not curToken.isPrimitive then
      begin
        if parentsCount >= length(parentsStack) then
          SetLength(parentsStack, parentsCount + 64);
        parentsStack[parentsCount] := curToken.tokenName;
        inc(parentsCount);
        continue;
      end;
      
      if (curToken.len <= 0) or not (curToken.state in [tsCorrect, tsError, tsSkipped]) then
        continue;
        
      tokenType := -1;
      tokenModifiers := 0;
      tName := curToken.tokenName;
      
      if tName = 'RW' then
      begin
        if (TReservedWord(curToken).kind >= rwAnd) and (TReservedWord(curToken).kind <= rwTry) then
        begin
          tokenType := 10; // keyword
        end
        else if TReservedWord(curToken).kind in [
          rwAssign, rwPlus, rwMinus, rwMultiply, rwExponentiation, rwDivide, rwHat,
          rwEquals, rwNotEqual, rwLess, rwMore, rwLessOrEqual, rwMoreOrEqual,
          rwAt, rwShl2, rwShr2, rwSymmetricDifference
        ] then
        begin
          tokenType := 14; // operator
        end;
      end
      else if tName = 'Num' then
      begin
        tokenType := 11; // number
      end
      else if tName = 'Str' then
      begin
        tokenType := 12; // string
      end
      else if (tName = 'Ident') or (tName = 'SymbDecl') or (tName = 'SymbRef') then
      begin
        ident := TIdentifier(curToken);
        sym := TSymbol(ident.symbol);
        
        if sym <> nil then
        begin
          if sym.declaration = ident then
            tokenModifiers := tokenModifiers or 1; // declaration
            
          isParam := sym.isParameter;
          
          case sym.kind of
            skUnitName:
              tokenType := 0; // namespace
            skTypeName:
              begin
                if (sym.typeDef <> nil) and (sym.typeDef.kind = tkClass) then
                  tokenType := 1 // class
                else if (sym.typeDef <> nil) and (sym.typeDef.kind = tkRecord) then
                  tokenType := 3 // struct
                else
                  tokenType := 4; // type
              end;
            skConstant, skTypedConstant:
              begin
                tokenType := 6; // variable
                tokenModifiers := tokenModifiers or 4; // readonly
              end;
            skVariable:
              begin
                for i := parentsCount - 1 downto 0 do
                  if parentsStack[i] = 'ParameterDecl' then
                    isParam := true;
                    
                if isParam then
                  tokenType := 5 // parameter
                else if (sym.parent <> nil) and (sym.parent.kind = skTypeName) then
                  tokenType := 7 // property
                else
                  tokenType := 6; // variable
              end;
            skProcedure, skFunction:
              begin
                if (sym.parent <> nil) and (sym.parent.kind = skTypeName) then
                  tokenType := 9 // method
                else
                  tokenType := 8; // function
              end;
            skConstructor, skDestructor:
              tokenType := 9; // method
          end;
        end
        else
        begin
          identName := LowerCase(ident.GetStr());
          if (identName = 'public') or (identName = 'protected') or
             (identName = 'private') or (identName = 'published') or
             (identName = 'strict') then
            tokenType := 10 // keyword
          else if TypesList.Find(identName) <> nil then
            tokenType := 4; // type
        end;
      end;
      
      if tokenType <> -1 then
      begin
        AddSemanticToken(tokens, count, capacity, curToken.line, curToken.position, curToken.len, tokenType, tokenModifiers);
      end;
    end;
    
    SetLength(parentsStack, 0);
  end;
  
  if count > 0 then
  begin
    QuickSortSemanticTokens(tokens, 0, count - 1);
  end;
  
  dataJson := '';
  prevLine := 0;
  prevChar := 0;
  for i := 0 to count - 1 do
  begin
    if i > 0 then
      dataJson := dataJson + ',';
      
    deltaLine := tokens[i].line - prevLine;
    if deltaLine > 0 then
      deltaChar := tokens[i].position
    else
      deltaChar := tokens[i].position - prevChar;
      
    dataJson := dataJson + 
      IntToStr(deltaLine) + ',' +
      IntToStr(deltaChar) + ',' +
      IntToStr(tokens[i].len) + ',' +
      IntToStr(tokens[i].tokenType) + ',' +
      IntToStr(tokens[i].tokenModifiers);
      
    prevLine := tokens[i].line;
    prevChar := tokens[i].position;
  end;
  
  SetLength(tokens, 0);
  
  Response := Response + '"result":{"data":[' + dataJson + ']}}';
  SendResponse(WriteStream, Response);
end;

end.
