unit LspCompletion;

{$mode objfpc}
{$longstrings on}

interface

uses
  sysutils, classes, fpjson, jsonparser,
  ParserContext, Symbols, TypeDefs, TypeDef, ClassTypeDef, ObjectTypeDef,
  RecordTypeDef, PointerTypeDef, ArrayTypeDef, DynamicArrayTypeDef, RoutineTypeDef,
  LspUtils, LspState;

procedure HandleCompletion(WriteStream: TStream; Id: TJSONData; Params: TJSONData);

implementation

function IsSameOrSubclass(CurrentClass, TargetClass: TTypeDef): boolean;
var
  c: TTypeDef;
begin
  if (CurrentClass = nil) or (TargetClass = nil) then exit(false);
  c := CurrentClass;
  while c <> nil do
  begin
    if c = TargetClass then exit(true);
    if (c.kind = tkClass) and (c is TClassTypeDef) then
      c := TClassTypeDef(c).parentClass
    else if (c.kind = tkObject) and (c is TObjectTypeDef) then
      c := TObjectTypeDef(c).parentObject
    else
      break;
  end;
  Result := false;
end;

procedure AddCompletionItem(var ItemsJson: string; AddedNames: TStringList; const MemberName: string; MemberType, CurrentClassType, TargetClassType: TTypeDef);
var
  LowerName: string;
  ItemKind: integer;
  Detail, ItemJson: string;
begin
  LowerName := LowerCase(MemberName);
  if AddedNames.IndexOf(LowerName) >= 0 then exit;

  if (MemberType <> nil) and (MemberType.visibility in [vPrivate, vProtected]) then
  begin
    if not IsSameOrSubclass(CurrentClassType, TargetClassType) then
      exit;
  end;

  AddedNames.Add(LowerName);

  if MemberType <> nil then
  begin
    if MemberType.kind in [tkProcedure, tkFunction] then
    begin
      ItemKind := 2; // Method
      if (MemberType.kind = tkFunction) and (MemberType is TRoutineTypeDef) and (TRoutineTypeDef(MemberType).returnType <> nil) then
        Detail := 'function: ' + TypeKindStr[ord(TRoutineTypeDef(MemberType).returnType.kind)]
      else if MemberType.kind = tkFunction then
        Detail := 'function'
      else
        Detail := 'procedure';
    end
    else
    begin
      ItemKind := 5; // Field
      Detail := TypeKindStr[ord(MemberType.kind)];
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
  CurrType, CType, OType, MemberType, CurrentClassType: TTypeDef;
  AddedNames: TStringList;
begin
  Response := '{"jsonrpc":"2.0",';
  if Id <> nil then
    Response := Response + '"id":' + Id.AsJSON + ','
  else
    Response := Response + '"id":null,';

  ItemsJson := '';

  if Params <> nil then
  begin
    Uri := Params.FindPath('textDocument.uri').AsString;
    TargetLine := Params.FindPath('position.line').AsInteger;
    TargetCharacter := Params.FindPath('position.character').AsInteger;

    EnsureParsed(WriteStream, Uri);

    if (LastParserContext <> nil) and (LastParsedUri = Uri) then
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
              if (CurrentClassType.kind = tkPointer) and (CurrentClassType is TPointerTypeDef) and (TPointerTypeDef(CurrentClassType).pointerToType <> nil) then
                CurrentClassType := TPointerTypeDef(CurrentClassType).pointerToType;
            end;

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
                  begin
                    if (CurrType is TRecordTypeDef) and (TRecordTypeDef(CurrType).recordFields <> nil) then
                    begin
                      Found := TRecordTypeDef(CurrType).recordFields.Find(NextIdent);
                      if Found = nil then
                        Found := TRecordTypeDef(CurrType).recordFields.Find(LowerCase(NextIdent));
                    end;
                  end;
                tkClass:
                  begin
                    CType := CurrType;
                    while CType <> nil do
                    begin
                      if (CType.kind = tkClass) and (CType is TClassTypeDef) and (TClassTypeDef(CType).classFields <> nil) then
                      begin
                        Found := TClassTypeDef(CType).classFields.Find(NextIdent);
                        if Found = nil then
                          Found := TClassTypeDef(CType).classFields.Find(LowerCase(NextIdent));
                        if Found <> nil then break;
                        CType := TClassTypeDef(CType).parentClass;
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
                      if (OType.kind = tkObject) and (OType is TObjectTypeDef) and (TObjectTypeDef(OType).objectFields <> nil) then
                      begin
                        Found := TObjectTypeDef(OType).objectFields.Find(NextIdent);
                        if Found = nil then
                          Found := TObjectTypeDef(OType).objectFields.Find(LowerCase(NextIdent));
                        if Found <> nil then break;
                        OType := TObjectTypeDef(OType).parentObject;
                      end
                      else
                        break;
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

              AddedNames := TStringList.Create;
              try
                case CurrType.kind of
                  tkRecord:
                    begin
                      if (CurrType is TRecordTypeDef) and (TRecordTypeDef(CurrType).recordFields <> nil) then
                      begin
                        for i := 0 to TRecordTypeDef(CurrType).recordFields.Count - 1 do
                        begin
                          IdentStr := TRecordTypeDef(CurrType).recordFields.NameOfIndex(i);
                          MemberType := TTypeDef(TRecordTypeDef(CurrType).recordFields.Items[i]);
                          AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CurrType);
                        end;
                      end;
                    end;
                  tkClass:
                    begin
                      CType := CurrType;
                      while CType <> nil do
                      begin
                        if (CType.kind = tkClass) and (CType is TClassTypeDef) and (TClassTypeDef(CType).classFields <> nil) then
                        begin
                          for i := 0 to TClassTypeDef(CType).classFields.Count - 1 do
                          begin
                            IdentStr := TClassTypeDef(CType).classFields.NameOfIndex(i);
                            MemberType := TTypeDef(TClassTypeDef(CType).classFields.Items[i]);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CType);
                          end;
                          CType := TClassTypeDef(CType).parentClass;
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
                        if (OType.kind = tkObject) and (OType is TObjectTypeDef) and (TObjectTypeDef(OType).objectFields <> nil) then
                        begin
                          for i := 0 to TObjectTypeDef(OType).objectFields.Count - 1 do
                          begin
                            IdentStr := TObjectTypeDef(OType).objectFields.NameOfIndex(i);
                            MemberType := TTypeDef(TObjectTypeDef(OType).objectFields.Items[i]);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, OType);
                          end;
                          OType := TObjectTypeDef(OType).parentObject;
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
