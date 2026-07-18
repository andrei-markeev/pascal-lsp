unit LspCompletion;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Symbols, TypeDefs,
  LspUtils, LspState;

procedure HandleCompletion(WriteStream: TStream; Id: TJSONData; Params: TJSONData);

implementation

function IsSameOrSubclass(CurrentClass, TargetClass: PTypeDef): boolean;
var
  c: PTypeDef;
begin
  if (CurrentClass = nil) or (TargetClass = nil) then exit(false);
  c := CurrentClass;
  while c <> nil do
  begin
    if c = TargetClass then exit(true);
    if c^.kind = tkClass then
      c := c^.parentClass
    else if c^.kind = tkObject then
      c := c^.parentObject
    else
      break;
  end;
  Result := false;
end;

procedure AddCompletionItem(var ItemsJson: string; AddedNames: TStringList; const MemberName: string; MemberType, CurrentClassType, TargetClassType: PTypeDef);
var
  LowerName: string;
  ItemKind: integer;
  Detail, ItemJson: string;
begin
  LowerName := LowerCase(MemberName);
  if AddedNames.IndexOf(LowerName) >= 0 then exit;

  if (MemberType <> nil) and (MemberType^.visibility in [vPrivate, vProtected]) then
  begin
    if not IsSameOrSubclass(CurrentClassType, TargetClassType) then
      exit;
  end;

  AddedNames.Add(LowerName);

  if MemberType <> nil then
  begin
    if MemberType^.kind in [tkProcedure, tkFunction] then
    begin
      ItemKind := 2; // Method
      if (MemberType^.kind = tkFunction) and (MemberType^.returnType <> nil) then
        Detail := 'function: ' + TypeKindStr[ord(MemberType^.returnType^.kind)]
      else if MemberType^.kind = tkFunction then
        Detail := 'function'
      else
        Detail := 'procedure';
    end
    else
    begin
      ItemKind := 5; // Field
      Detail := TypeKindStr[ord(MemberType^.kind)];
    end;
  end
  else
  begin
    ItemKind := 5;
    Detail := '';
  end;

  if ItemsJson <> '' then
    ItemsJson := ItemsJson + ',';

  ItemJson := '{' +
    '"label":' + '"' + string(StringToJSONString(MemberName)) + '",' +
    '"kind":' + IntToStr(ItemKind);
  if Detail <> '' then
    ItemJson := ItemJson + ',"detail":' + '"' + string(StringToJSONString(Detail)) + '"';
  ItemJson := ItemJson + '}';

  ItemsJson := ItemsJson + ItemJson;
end;

procedure HandleCompletion(WriteStream: TStream; Id: TJSONData; Params: TJSONData);
var
  Uri, Content, Response, ItemsJson: string;
  TargetLine, TargetCharacter, P, idx, depth, chainCount, k, i: integer;
  FilterPrefix, IdentStr, RootIdent, NextIdent: string;
  Chain: array of string;
  CursorPChar: PChar;
  Sym, SelfSym: TSymbol;
  Found: pointer;
  CurrType, CType, OType, MemberType, CurrentClassType: PTypeDef;
  AddedNames: TStringList;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';

  ItemsJson := '';

  if (Params <> nil) and (LastParserContext <> nil) then
  begin
    Uri := Params.FindPath('textDocument.uri').AsString;
    TargetLine := Params.FindPath('position.line').AsInteger;
    TargetCharacter := Params.FindPath('position.character').AsInteger;

    if LastParsedUri = Uri then
    begin
      Content := LastParserContext.GetContents;
      P := LineCharToOffset(Content, TargetLine, TargetCharacter);

      if P > 1 then
      begin
        idx := P - 1;

        FilterPrefix := '';
        while (idx >= 1) and (Content[idx] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
        begin
          FilterPrefix := Content[idx] + FilterPrefix;
          dec(idx);
        end;

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

            IdentStr := '';
            while (idx >= 1) and (Content[idx] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
            begin
              IdentStr := Content[idx] + IdentStr;
              dec(idx);
            end;

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
            CursorPChar := PChar(Content) + (P - 1);
            Sym := FindSymbol(RootIdent, CursorPChar);

            SelfSym := FindSymbol('Self', CursorPChar);
            CurrentClassType := nil;
            if (SelfSym <> nil) and (SelfSym.typeDef <> nil) then
            begin
              CurrentClassType := SelfSym.typeDef;
              if (CurrentClassType^.kind = tkPointer) and (CurrentClassType^.pointerToType <> nil) then
                CurrentClassType := CurrentClassType^.pointerToType;
            end;

            CurrType := nil;
            if (Sym <> nil) and (Sym.typeDef <> nil) then
              CurrType := Sym.typeDef
            else
            begin
              Found := TypesList.Find(LowerCase(RootIdent));
              if Found <> nil then
                CurrType := PTypeDef(Found);
            end;

            for k := chainCount - 2 downto 0 do
            begin
              if CurrType = nil then break;

              if (CurrType^.kind = tkPointer) and (CurrType^.pointerToType <> nil) then
                CurrType := CurrType^.pointerToType;

              if (CurrType^.kind = tkFunction) and (CurrType^.returnType <> nil) then
                CurrType := CurrType^.returnType
              else if (CurrType^.kind = tkArray) and (CurrType^.typeOfValues <> nil) then
                CurrType := CurrType^.typeOfValues
              else if (CurrType^.kind = tkDynamicArray) and (CurrType^.typeOfDynValues <> nil) then
                CurrType := CurrType^.typeOfDynValues;

              NextIdent := Chain[k];
              Found := nil;

              case CurrType^.kind of
                tkRecord:
                  begin
                    if CurrType^.recordFields <> nil then
                    begin
                      Found := CurrType^.recordFields.Find(NextIdent);
                      if Found = nil then
                        Found := CurrType^.recordFields.Find(LowerCase(NextIdent));
                    end;
                  end;
                tkClass:
                  begin
                    CType := CurrType;
                    while CType <> nil do
                    begin
                      if (CType^.kind = tkClass) and (CType^.classFields <> nil) then
                      begin
                        Found := CType^.classFields.Find(NextIdent);
                        if Found = nil then
                          Found := CType^.classFields.Find(LowerCase(NextIdent));
                        if Found <> nil then break;
                        CType := CType^.parentClass;
                      end
                      else
                        break;
                    end;
                  end;
                tkObject:
                  begin
                    OType := CurrType;
                    while OType <> nil do
                    begin
                      if (OType^.kind = tkObject) and (OType^.objectFields <> nil) then
                      begin
                        Found := OType^.objectFields.Find(NextIdent);
                        if Found = nil then
                          Found := OType^.objectFields.Find(LowerCase(NextIdent));
                        if Found <> nil then break;
                        OType := OType^.parentObject;
                      end
                      else
                        break;
                    end;
                  end;
              end;

              if Found <> nil then
                CurrType := PTypeDef(Found)
              else
              begin
                CurrType := nil;
                break;
              end;
            end;

            if CurrType <> nil then
            begin
              if (CurrType^.kind = tkPointer) and (CurrType^.pointerToType <> nil) then
                CurrType := CurrType^.pointerToType;

              if (CurrType^.kind = tkFunction) and (CurrType^.returnType <> nil) then
                CurrType := CurrType^.returnType
              else if (CurrType^.kind = tkArray) and (CurrType^.typeOfValues <> nil) then
                CurrType := CurrType^.typeOfValues
              else if (CurrType^.kind = tkDynamicArray) and (CurrType^.typeOfDynValues <> nil) then
                CurrType := CurrType^.typeOfDynValues;

              AddedNames := TStringList.Create;
              try
                case CurrType^.kind of
                  tkRecord:
                    begin
                      if CurrType^.recordFields <> nil then
                      begin
                        for i := 0 to CurrType^.recordFields.Count - 1 do
                        begin
                          IdentStr := CurrType^.recordFields.NameOfIndex(i);
                          MemberType := PTypeDef(CurrType^.recordFields.Items[i]);
                          AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CurrType);
                        end;
                      end;
                    end;
                  tkClass:
                    begin
                      CType := CurrType;
                      while CType <> nil do
                      begin
                        if (CType^.kind = tkClass) and (CType^.classFields <> nil) then
                        begin
                          for i := 0 to CType^.classFields.Count - 1 do
                          begin
                            IdentStr := CType^.classFields.NameOfIndex(i);
                            MemberType := PTypeDef(CType^.classFields.Items[i]);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CType);
                          end;
                          CType := CType^.parentClass;
                        end
                        else
                          break;
                      end;
                    end;
                  tkObject:
                    begin
                      OType := CurrType;
                      while OType <> nil do
                      begin
                        if (OType^.kind = tkObject) and (OType^.objectFields <> nil) then
                        begin
                          for i := 0 to OType^.objectFields.Count - 1 do
                          begin
                            IdentStr := OType^.objectFields.NameOfIndex(i);
                            MemberType := PTypeDef(OType^.objectFields.Items[i]);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, OType);
                          end;
                          OType := OType^.parentObject;
                        end
                        else
                          break;
                      end;
                    end;
                end;
              finally
                AddedNames.Free;
              end;
            end;
          end;
        end;
      end;
    end;
  end;

  Response := Response + '"result":[' + ItemsJson + ']}';
  SendResponse(WriteStream, Response);
end;

end.
