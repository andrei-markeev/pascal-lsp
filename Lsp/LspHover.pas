unit LspHover;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser, contnrs,
  ParserContext, Token, Identifier, Symbols, ReservedWord, TypeDefs, TypeDef,
  ClassTypeDef, ObjectTypeDef, RecordTypeDef, PointerTypeDef, ArrayTypeDef,
  DynamicArrayTypeDef, SetTypeDef, RoutineTypeDef, Parameters, EnumTypeDef, EnumMemberTypeDef,
  LspUtils, LspState;

procedure HandleHover(WriteStream: TStream; Id: TJSONData; Params: TJSONData);

implementation

function GetTypeDefDisplay(typeDef: TTypeDef; IgnoreSymbol: TSymbol = nil): string;
var
  i, count, limit: integer;
begin
  if (typeDef = nil) or (typeDef = unknownType) then
    exit('unknown');

  if (typeDef.typeSymbol <> nil) and (TSymbol(typeDef.typeSymbol) <> IgnoreSymbol) and (TSymbol(typeDef.typeSymbol).displayName <> '') then
    exit(string(TSymbol(typeDef.typeSymbol).displayName));

  case typeDef.kind of
    tkInteger: Result := 'Integer';
    tkBoolean: Result := 'Boolean';
    tkChar: Result := 'Char';
    tkCharRange: Result := 'char range';
    tkEnum:
      begin
        if (typeDef is TEnumTypeDef) and (Length(TEnumTypeDef(typeDef).members) > 0) then
        begin
          Result := '(';
          count := Length(TEnumTypeDef(typeDef).members);
          limit := count;
          if limit > 10 then
            limit := 10;
          for i := 0 to limit - 1 do
          begin
            if i > 0 then Result := Result + ', ';
            Result := Result + TEnumTypeDef(typeDef).members[i];
          end;
          if count > 10 then
            Result := Result + ', ...';
          Result := Result + ')';
        end
        else
          Result := 'enumeration';
      end;
    tkEnumMember:
      begin
        if (typeDef is TEnumMemberTypeDef) and (TEnumMemberTypeDef(typeDef).enumType <> nil) then
          Result := GetTypeDefDisplay(TEnumMemberTypeDef(typeDef).enumType)
        else
          Result := 'enum';
      end;
    tkReal: Result := 'Real';
    tkString: Result := 'String';
    tkPointer:
      begin
        if (typeDef is TPointerTypeDef) and (TPointerTypeDef(typeDef).pointerToType <> nil) then
          Result := '^' + GetTypeDefDisplay(TPointerTypeDef(typeDef).pointerToType)
        else
          Result := 'Pointer';
      end;
    tkArray:
      begin
        if (typeDef is TArrayTypeDef) and (TArrayTypeDef(typeDef).typeOfValues <> nil) then
          Result := 'array of ' + GetTypeDefDisplay(TArrayTypeDef(typeDef).typeOfValues)
        else
          Result := 'array';
      end;
    tkDynamicArray:
      begin
        if (typeDef is TDynamicArrayTypeDef) and (TDynamicArrayTypeDef(typeDef).typeOfDynValues <> nil) then
          Result := 'array of ' + GetTypeDefDisplay(TDynamicArrayTypeDef(typeDef).typeOfDynValues)
        else
          Result := 'array';
      end;
    tkSet:
      begin
        if (typeDef is TSetTypeDef) and (TSetTypeDef(typeDef).typeOfSet <> nil) then
          Result := 'set of ' + GetTypeDefDisplay(TSetTypeDef(typeDef).typeOfSet)
        else
          Result := 'set';
      end;
    tkRecord: Result := 'record';
    tkObject: Result := 'object';
    tkClass: Result := 'class';
    tkProcedure: Result := 'procedure';
    tkFunction: Result := 'function';
    tkUnitName: Result := 'unit';
    tkUnknown: Result := 'unknown';
  else
    if (ord(typeDef.kind) >= 0) and (ord(typeDef.kind) < NUM_OF_TYPE_KINDS) then
      Result := TypeKindStr[ord(typeDef.kind)]
    else
      Result := 'unknown';
  end;
end;

function FormatRoutineSig(const Keyword, Name: string; RoutineTypeDef: TRoutineTypeDef): string;
var
  ParamList: TParameterList;
  i, idx: integer;
  ParamStr, ParamItem, KindPrefix: string;
  OverloadType: TTypeDef;
begin
  ParamStr := '';
  if (RoutineTypeDef <> nil) and (RoutineTypeDef.parameters <> nil) then
  begin
    ParamList := TParameterList(RoutineTypeDef.parameters);
    for i := 0 to ParamList.count - 1 do
    begin
      if i > 0 then ParamStr := ParamStr + '; ';
      case ParamList.items[i].kind of
        ptkConst: KindPrefix := 'const ';
        ptkVar: KindPrefix := 'var ';
        ptkOut: KindPrefix := 'out ';
      else
        KindPrefix := '';
      end;
      ParamItem := KindPrefix + ParamList.items[i].name;
      if ParamList.items[i].typeDef <> nil then
        ParamItem := ParamItem + ': ' + GetTypeDefDisplay(ParamList.items[i].typeDef);
      if ParamList.items[i].hasDefaultValue then
        ParamItem := ParamItem + ' = ...';
      ParamStr := ParamStr + ParamItem;
    end;
  end;

  if ParamStr <> '' then
    ParamStr := '(' + ParamStr + ')';

  Result := Keyword;
  if Name <> '' then
    Result := Result + ' ' + Name;
  Result := Result + ParamStr;

  if (RoutineTypeDef <> nil) and (RoutineTypeDef.kind = tkFunction) and (RoutineTypeDef.returnType <> nil) then
    Result := Result + ': ' + GetTypeDefDisplay(RoutineTypeDef.returnType);

  Result := Result + ';';

  if (RoutineTypeDef <> nil) and (RoutineTypeDef.overloads <> nil) then
  begin
    for idx := 0 to RoutineTypeDef.overloads.Count - 1 do
    begin
      OverloadType := TTypeDef(RoutineTypeDef.overloads.Items[idx]);
      if (OverloadType <> nil) and (OverloadType is TRoutineTypeDef) then
      begin
        if OverloadType.kind = tkFunction then
          Result := Result + #10 + FormatRoutineSig('function', Name, TRoutineTypeDef(OverloadType))
        else
          Result := Result + #10 + FormatRoutineSig('procedure', Name, TRoutineTypeDef(OverloadType));
      end;
    end;
  end;
end;

function GetSymbolHoverText(Sym: TSymbol; out LabelText: string): string;
begin
  LabelText := '';
  case Sym.kind of
    skVariable:
      begin
        if Sym.isParameter then
        begin
          LabelText := '*(parameter)*';
          Result := 'var ' + string(Sym.displayName) + ': ' + GetTypeDefDisplay(Sym.typeDef) + ';';
        end
        else if (Sym.parent <> nil) and (Sym.parent.kind = skTypeName) then
        begin
          LabelText := '*(field)*';
          Result := 'var ' + string(Sym.displayName) + ': ' + GetTypeDefDisplay(Sym.typeDef) + ';';
        end
        else
          Result := 'var ' + string(Sym.displayName) + ': ' + GetTypeDefDisplay(Sym.typeDef) + ';';
      end;
    skConstant, skTypedConstant:
      begin
        if Sym.typeDef <> nil then
          Result := 'const ' + string(Sym.displayName) + ': ' + GetTypeDefDisplay(Sym.typeDef) + ';'
        else
          Result := 'const ' + string(Sym.displayName) + ';';
      end;
    skTypeName:
      begin
        if Sym.typeDef <> nil then
          Result := 'type ' + string(Sym.displayName) + ' = ' + GetTypeDefDisplay(Sym.typeDef, Sym) + ';'
        else
          Result := 'type ' + string(Sym.displayName) + ';';
      end;
    skProcedure:
      begin
        if (Sym.typeDef <> nil) and (Sym.typeDef is TRoutineTypeDef) then
          Result := FormatRoutineSig('procedure', string(Sym.displayName), TRoutineTypeDef(Sym.typeDef))
        else
          Result := 'procedure ' + string(Sym.displayName) + ';';
      end;
    skFunction:
      begin
        if (Sym.typeDef <> nil) and (Sym.typeDef is TRoutineTypeDef) then
          Result := FormatRoutineSig('function', string(Sym.displayName), TRoutineTypeDef(Sym.typeDef))
        else
          Result := 'function ' + string(Sym.displayName) + ';';
      end;
    skConstructor:
      begin
        if (Sym.typeDef <> nil) and (Sym.typeDef is TRoutineTypeDef) then
          Result := FormatRoutineSig('constructor', string(Sym.displayName), TRoutineTypeDef(Sym.typeDef))
        else
          Result := 'constructor ' + string(Sym.displayName) + ';';
      end;
    skDestructor:
      begin
        if (Sym.typeDef <> nil) and (Sym.typeDef is TRoutineTypeDef) then
          Result := FormatRoutineSig('destructor', string(Sym.displayName), TRoutineTypeDef(Sym.typeDef))
        else
          Result := 'destructor ' + string(Sym.displayName) + ';';
      end;
    skUnitName:
      begin
        Result := 'unit ' + string(Sym.displayName) + ';';
      end;
  else
    Result := string(Sym.displayName) + ';';
  end;
end;

function ResolveMemberHover(const Content: string; TargetLine, TargetCharacter: integer; TargetIdent: TIdentifier; out LabelText, CodeSnippet: string): boolean;
var
  P, idx, chainCount, depth, identEnd, k: integer;
  Chain: array of string;
  IdentStr, RootIdent, NextIdent: string;
  CursorPChar: PChar;
  Sym: TSymbol;
  Found, FoundMember: pointer;
  CurrType, CType, OType, MemberType: TTypeDef;
begin
  Result := false;
  LabelText := '';
  CodeSnippet := '';

  P := LineCharToOffset(Content, TargetLine, TargetCharacter);
  if P <= 1 then exit;

  idx := P - TargetIdent.len - 1;
  while (idx >= 1) and (Content[idx] in [' ', #9]) do
    dec(idx);

  if (idx >= 1) and (Content[idx] = '.') then
  begin
    dec(idx);
    chainCount := 0;
    SetLength(Chain, 16);

    while idx >= 1 do
    begin
      while (idx >= 1) and (Content[idx] in [' ', #9]) do
        dec(idx);

      if (idx >= 1) and (Content[idx] = ')') then
      begin
        dec(idx);
        depth := 1;
        while (idx >= 1) and (depth > 0) do
        begin
          if Content[idx] = ')' then inc(depth)
          else if Content[idx] = '(' then dec(depth);
          dec(idx);
        end;
        while (idx >= 1) and (Content[idx] in [' ', #9]) do dec(idx);
      end
      else if (idx >= 1) and (Content[idx] = ']') then
      begin
        dec(idx);
        depth := 1;
        while (idx >= 1) and (depth > 0) do
        begin
          if Content[idx] = ']' then inc(depth)
          else if Content[idx] = '[' then dec(depth);
          dec(idx);
        end;
        while (idx >= 1) and (Content[idx] in [' ', #9]) do dec(idx);
      end;

      if (idx >= 1) and (Content[idx] = '^') then
        dec(idx);

      identEnd := idx;
      while (idx >= 1) and (Content[idx] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        dec(idx);

      if identEnd > idx then
        IdentStr := Copy(Content, idx + 1, identEnd - idx)
      else
        IdentStr := '';

      if IdentStr = '' then break;

      if chainCount >= Length(Chain) then
        SetLength(Chain, chainCount + 16);
      Chain[chainCount] := IdentStr;
      inc(chainCount);

      while (idx >= 1) and (Content[idx] in [' ', #9]) do
        dec(idx);

      if (idx >= 1) and (Content[idx] = '.') then
        dec(idx)
      else
        break;
    end;

    if chainCount > 0 then
    begin
      RootIdent := Chain[chainCount - 1];
      CursorPChar := TargetIdent.start;
      Sym := FindSymbol(RootIdent, CursorPChar);

      CurrType := nil;
      if (Sym <> nil) and (Sym.typeDef <> nil) then
        CurrType := Sym.typeDef
      else
      begin
        Found := TypesList.Find(LowerCase(RootIdent));
        if Found <> nil then
          CurrType := TTypeDef(Found);
      end;

      for k := chainCount - 2 downto 0 do
      begin
        if CurrType = nil then break;

        if (CurrType.kind = tkPointer) and (CurrType is TPointerTypeDef) and (TPointerTypeDef(CurrType).pointerToType <> nil) then
          CurrType := TPointerTypeDef(CurrType).pointerToType;

        if (CurrType.kind = tkFunction) and (CurrType is TRoutineTypeDef) and (TRoutineTypeDef(CurrType).returnType <> nil) then
          CurrType := TRoutineTypeDef(CurrType).returnType
        else if (CurrType.kind = tkArray) and (CurrType is TArrayTypeDef) and (TArrayTypeDef(CurrType).typeOfValues <> nil) then
          CurrType := TArrayTypeDef(CurrType).typeOfValues
        else if (CurrType.kind = tkDynamicArray) and (CurrType is TDynamicArrayTypeDef) and (TDynamicArrayTypeDef(CurrType).typeOfDynValues <> nil) then
          CurrType := TDynamicArrayTypeDef(CurrType).typeOfDynValues;

        NextIdent := Chain[k];
        Found := nil;

        case CurrType.kind of
          tkRecord:
            if CurrType is TRecordTypeDef then
              Found := TRecordTypeDef(CurrType).FindMember(NextIdent);
          tkClass:
            begin
              CType := CurrType;
              while CType <> nil do
              begin
                if (CType.kind = tkClass) and (CType is TClassTypeDef) then
                begin
                  Found := TClassTypeDef(CType).FindMember(NextIdent);
                  if Found <> nil then break;
                  CType := TClassTypeDef(CType).parentClass;
                end
                else break;
              end;
            end;
          tkObject:
            begin
              OType := CurrType;
              while OType <> nil do
              begin
                if (OType.kind = tkObject) and (OType is TObjectTypeDef) then
                begin
                  Found := TObjectTypeDef(OType).FindMember(NextIdent);
                  if Found <> nil then break;
                  OType := TObjectTypeDef(OType).parentObject;
                end
                else break;
              end;
            end;
        end;

        if Found <> nil then
          CurrType := TTypeDef(Found)
        else
        begin
          CurrType := nil;
          break;
        end;
      end;

      if CurrType <> nil then
      begin
        if (CurrType.kind = tkPointer) and (CurrType is TPointerTypeDef) and (TPointerTypeDef(CurrType).pointerToType <> nil) then
          CurrType := TPointerTypeDef(CurrType).pointerToType;

        if (CurrType.kind = tkFunction) and (CurrType is TRoutineTypeDef) and (TRoutineTypeDef(CurrType).returnType <> nil) then
          CurrType := TRoutineTypeDef(CurrType).returnType
        else if (CurrType.kind = tkArray) and (CurrType is TArrayTypeDef) and (TArrayTypeDef(CurrType).typeOfValues <> nil) then
          CurrType := TArrayTypeDef(CurrType).typeOfValues
        else if (CurrType.kind = tkDynamicArray) and (CurrType is TDynamicArrayTypeDef) and (TDynamicArrayTypeDef(CurrType).typeOfDynValues <> nil) then
          CurrType := TDynamicArrayTypeDef(CurrType).typeOfDynValues;

        FoundMember := nil;
        case CurrType.kind of
          tkRecord:
            if CurrType is TRecordTypeDef then
              FoundMember := TRecordTypeDef(CurrType).FindMember(TargetIdent.GetStr());
          tkClass:
            begin
              CType := CurrType;
              while CType <> nil do
              begin
                if (CType.kind = tkClass) and (CType is TClassTypeDef) then
                begin
                  FoundMember := TClassTypeDef(CType).FindMember(TargetIdent.GetStr());
                  if FoundMember <> nil then break;
                  CType := TClassTypeDef(CType).parentClass;
                end
                else break;
              end;
            end;
          tkObject:
            begin
              OType := CurrType;
              while OType <> nil do
              begin
                if (OType.kind = tkObject) and (OType is TObjectTypeDef) then
                begin
                  FoundMember := TObjectTypeDef(OType).FindMember(TargetIdent.GetStr());
                  if FoundMember <> nil then break;
                  OType := TObjectTypeDef(OType).parentObject;
                end
                else break;
              end;
            end;
        end;

        if FoundMember <> nil then
        begin
          LabelText := '';
          MemberType := TTypeDef(FoundMember);
          if MemberType.kind in [tkProcedure, tkFunction] then
          begin
            if (MemberType.kind = tkFunction) and (MemberType is TRoutineTypeDef) then
              CodeSnippet := FormatRoutineSig('function', TargetIdent.GetStr(), TRoutineTypeDef(MemberType))
            else if MemberType is TRoutineTypeDef then
              CodeSnippet := FormatRoutineSig('procedure', TargetIdent.GetStr(), TRoutineTypeDef(MemberType))
            else if MemberType.kind = tkFunction then
              CodeSnippet := 'function ' + TargetIdent.GetStr() + ';'
            else
              CodeSnippet := 'procedure ' + TargetIdent.GetStr() + ';';
          end
          else
          begin
            LabelText := '*(field)*';
            CodeSnippet := 'var ' + TargetIdent.GetStr() + ': ' + GetTypeDefDisplay(MemberType) + ';';
          end;
          exit(true);
        end;
      end;
    end;
  end;
end;

procedure HandleHover(WriteStream: TStream; Id: TJSONData; Params: TJSONData);
var
  Uri, ResultJson, Response, HoverText, LabelText, MarkdownContent, Content: string;
  TargetLine, TargetCharacter, i: integer;
  TargetIdent: TIdentifier;
  curToken: TToken;
  Sym: TSymbol;
  FoundType: pointer;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';

  ResultJson := 'null';

  if Params <> nil then
  begin
    Uri := Params.FindPath('textDocument.uri').AsString;
    TargetLine := Params.FindPath('position.line').AsInteger;
    TargetCharacter := Params.FindPath('position.character').AsInteger;

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
        HoverText := '';
        LabelText := '';
        if TargetIdent.symbol <> nil then
        begin
          Sym := TSymbol(TargetIdent.symbol);
          HoverText := GetSymbolHoverText(Sym, LabelText);
        end
        else
        begin
          Sym := FindSymbol(TargetIdent.GetStr(), TargetIdent.start);
          if Sym <> nil then
            HoverText := GetSymbolHoverText(Sym, LabelText)
          else
          begin
            Content := LastParserContext.GetContents;
            if not ResolveMemberHover(Content, TargetLine, TargetCharacter, TargetIdent, LabelText, HoverText) then
            begin
              FoundType := TypesList.Find(LowerCase(TargetIdent.GetStr()));
              if FoundType <> nil then
                HoverText := 'type ' + TargetIdent.GetStr() + ';'
              else if LoadedUnits.Find(LowerCase(TargetIdent.GetStr())) <> nil then
                HoverText := 'unit ' + TargetIdent.GetStr() + ';';
            end;
          end;
        end;

        if HoverText <> '' then
        begin
          if LabelText <> '' then
          begin
            ResultJson := '{' +
              '"contents":[' +
                '"' + string(StringToJSONString(LabelText)) + '",' +
                '{' +
                  '"language":"pascal",' +
                  '"value":"' + string(StringToJSONString(HoverText)) + '"' +
                '}' +
              '],' +
              '"range":{' +
                '"start":{"line":' + IntToStr(TargetIdent.line) + ',"character":' + IntToStr(TargetIdent.position) + '},' +
                '"end":{"line":' + IntToStr(TargetIdent.line) + ',"character":' + IntToStr(TargetIdent.position + TargetIdent.len) + '}' +
              '}' +
            '}';
          end
          else
          begin
            ResultJson := '{' +
              '"contents":{' +
                '"language":"pascal",' +
                '"value":"' + string(StringToJSONString(HoverText)) + '"' +
              '},' +
              '"range":{' +
                '"start":{"line":' + IntToStr(TargetIdent.line) + ',"character":' + IntToStr(TargetIdent.position) + '},' +
                '"end":{"line":' + IntToStr(TargetIdent.line) + ',"character":' + IntToStr(TargetIdent.position + TargetIdent.len) + '}' +
              '}' +
            '}';
          end;
        end;
      end;
    end;
  end;

  Response := Response + '"result":' + ResultJson + '}';
  SendResponse(WriteStream, Response);
end;

end.
