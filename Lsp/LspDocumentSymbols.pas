unit LspDocumentSymbols;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Token, Identifier, Symbols, Scopes, ProgramFile,
  LspUtils, LspState;

procedure HandleDocumentSymbol(WriteStream: TStream; const Uri: string; Id: TJSONData);

implementation

function IsInsideBlock(ident: TIdentifier; block: TToken): boolean;
begin
  Result := (ident <> nil) and (block <> nil) and 
            (ident.start >= block.start) and (ident.start < block.start + block.len);
end;

function IsMethodImplementation(symbol: TSymbol): boolean;
begin
  Result := (symbol.parent <> nil) and 
            (symbol.kind in [skProcedure, skFunction, skConstructor, skDestructor]) and
            ((symbol.parent.rangeToken = nil) or 
             (symbol.declaration.start < symbol.parent.rangeToken.start) or 
             (symbol.declaration.start >= symbol.parent.rangeToken.start + symbol.parent.rangeToken.len));
end;

function SerializeSymbol(symbol: TSymbol; interfaceBlock, implementationBlock: TToken): string;
var
  childJson, resultJson, symRange, selectionRange: string;
  symbolKindVal: integer;
  c: integer;
begin
  childJson := '';
  for c := 0 to length(symbol.children) - 1 do
  begin
    if (symbol.children[c] <> nil) and (symbol.children[c].declaration <> nil) then
    begin
      if not IsMethodImplementation(symbol.children[c]) and not symbol.children[c].isParameter then
      begin
        if childJson <> '' then
          childJson := childJson + ',';
        childJson := childJson + SerializeSymbol(symbol.children[c], interfaceBlock, implementationBlock);
      end;
    end;
  end;

  case symbol.kind of
    skUnitName: symbolKindVal := 4; // Package
    skTypeName: symbolKindVal := 5; // Class
    skConstant, skTypedConstant: symbolKindVal := 14; // Constant
    skVariable: symbolKindVal := 13; // Variable
    skProcedure, skFunction:
      begin
        if symbol.parent <> nil then
          symbolKindVal := 6 // Method
        else
          symbolKindVal := 12; // Function
      end;
    skConstructor, skDestructor: symbolKindVal := 9; // Constructor
  else
    symbolKindVal := 13; // Variable/Default
  end;

  if (symbol.rangeToken <> nil) and (symbol.rangeToken.endMarker <> nil) then
    symRange := '{"start":{"line":' + IntToStr(symbol.rangeToken.line) + ',"character":' + IntToStr(symbol.rangeToken.position) + '},' +
                '"end":{"line":' + IntToStr(symbol.rangeToken.endMarker.line) + ',"character":' + IntToStr(symbol.rangeToken.endMarker.position) + '}}'
  else
    symRange := '{"start":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position) + '},' +
                '"end":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position + symbol.declaration.len) + '}}';

  selectionRange := '{"start":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position) + '},' +
                     '"end":{"line":' + IntToStr(symbol.declaration.line) + ',"character":' + IntToStr(symbol.declaration.position + symbol.declaration.len) + '}}';

  resultJson := '{' +
    '"name":' + '"' + string(StringToJSONString(symbol.displayName)) + '"' + ',' +
    '"kind":' + IntToStr(symbolKindVal) + ',' +
    '"range":' + symRange + ',' +
    '"selectionRange":' + selectionRange;

  if childJson <> '' then
    resultJson := resultJson + ',"children":[' + childJson + ']';

  resultJson := resultJson + '}';
  Result := resultJson;
end;

procedure HandleDocumentSymbol(WriteStream: TStream; const Uri: string; Id: TJSONData);
var
  Response: string;
  i, j, k: integer;
  scope: TScope;
  symbol: TSymbol;
  symRange: string;
  interfaceBlock, implementationBlock, curToken: TToken;
  interfaceChildren, implementationChildren, topLevelJson: string;
  symbolSerialized: string;
  isInsideInterface, isInsideImplementation: boolean;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';
    
  interfaceBlock := nil;
  implementationBlock := nil;
  interfaceChildren := '';
  implementationChildren := '';
  topLevelJson := '';
  
  EnsureParsed(WriteStream, Uri);
  
  if (LastParserContext <> nil) and (LastParsedUri = Uri) then
  begin
    for k := 0 to LastParserContext.tokensLen - 1 do
    begin
      curToken := LastParserContext.Tokens[k];
      if curToken.ClassNameIs('TInterfaceBlock') then
        interfaceBlock := curToken
      else if curToken.ClassNameIs('TImplementationBlock') then
        implementationBlock := curToken;
    end;

    for i := 0 to length(ScopesList) - 1 do
    begin
      if (i > 0) and ((LastFileToken = nil) or not (LastFileToken is TProgramFile) or (i > 1)) then
        continue;
      scope := ScopesList[i];
      for j := 0 to scope.symbolsList.Count - 1 do
      begin
        symbol := TSymbol(scope.symbolsList.Items[j]);
        if (symbol <> nil) and (symbol.declaration <> nil) then
        begin
          if symbol.isParameter then
            continue;

          isInsideInterface := IsInsideBlock(symbol.declaration, interfaceBlock);
          isInsideImplementation := IsInsideBlock(symbol.declaration, implementationBlock);

          if isInsideInterface and (symbol.parent = nil) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if interfaceChildren <> '' then
              interfaceChildren := interfaceChildren + ',';
            interfaceChildren := interfaceChildren + symbolSerialized;
          end
          else if isInsideImplementation and ((symbol.parent = nil) or IsMethodImplementation(symbol)) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if implementationChildren <> '' then
              implementationChildren := implementationChildren + ',';
            implementationChildren := implementationChildren + symbolSerialized;
          end
          else if not isInsideInterface and not isInsideImplementation and ((symbol.parent = nil) or IsMethodImplementation(symbol)) then
          begin
            symbolSerialized := SerializeSymbol(symbol, interfaceBlock, implementationBlock);
            if topLevelJson <> '' then
              topLevelJson := topLevelJson + ',';
            topLevelJson := topLevelJson + symbolSerialized;
          end;
        end;
      end;
    end;
  end;

  if (interfaceBlock <> nil) and (interfaceBlock.endMarker <> nil) then
  begin
    symRange := '{"start":{"line":' + IntToStr(interfaceBlock.line) + ',"character":' + IntToStr(interfaceBlock.position) + '},' +
                '"end":{"line":' + IntToStr(interfaceBlock.endMarker.line) + ',"character":' + IntToStr(interfaceBlock.endMarker.position) + '}}';
    if topLevelJson <> '' then
      topLevelJson := topLevelJson + ',';
    topLevelJson := topLevelJson + '{' +
      '"name":"interface",' +
      '"kind":3,' + // Namespace
      '"range":' + symRange + ',' +
      '"selectionRange":' + symRange;
    if interfaceChildren <> '' then
      topLevelJson := topLevelJson + ',"children":[' + interfaceChildren + ']';
    topLevelJson := topLevelJson + '}';
  end;

  if (implementationBlock <> nil) and (implementationBlock.endMarker <> nil) then
  begin
    symRange := '{"start":{"line":' + IntToStr(implementationBlock.line) + ',"character":' + IntToStr(implementationBlock.position) + '},' +
                '"end":{"line":' + IntToStr(implementationBlock.endMarker.line) + ',"character":' + IntToStr(implementationBlock.endMarker.position) + '}}';
    if topLevelJson <> '' then
      topLevelJson := topLevelJson + ',';
    topLevelJson := topLevelJson + '{' +
      '"name":"implementation",' +
      '"kind":3,' + // Namespace
      '"range":' + symRange + ',' +
      '"selectionRange":' + symRange;
    if implementationChildren <> '' then
      topLevelJson := topLevelJson + ',"children":[' + implementationChildren + ']';
    topLevelJson := topLevelJson + '}';
  end;
  
  Response := Response + '"result":[' + topLevelJson + ']}';
  SendResponse(WriteStream, Response);
end;

end.
