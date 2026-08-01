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

procedure AddCompletionItem(var ItemsJson: string; AddedNames: TStringList; const MemberName: string; MemberType, CurrentClassType, TargetClassType: TTypeDef; AccessCtx: TParserContext = nil; CursorPChar: PChar = nil);
var
  LowerName: string;
  ItemKind: integer;
  Detail, ItemJson: string;
begin
  LowerName := LowerCase(MemberName);
  if AddedNames.IndexOf(LowerName) >= 0 then exit;

  if (MemberType <> nil) and (MemberType.visibility in [vPrivate, vProtected]) then
  begin
    if not IsMemberAccessible(AccessCtx, TargetClassType, MemberType.visibility, CursorPChar) then
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
  TargetLine, TargetCharacter, P, idx, depth, chainCount, k, i, identEnd: integer;
  IdentStr, RootIdent, NextIdent: string;
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

        while (idx >= 1) and (Content[idx] in ['a'..'z', 'A'..'Z', '_', '0'..'9']) do
          dec(idx);

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
                    if CurrType is TRecordTypeDef then
                      Found := TRecordTypeDef(CurrType).FindMember(NextIdent);
                  end;
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
                      else
                        break;
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
              AddedNames.Sorted := true;
              AddedNames.Duplicates := dupIgnore;
              try
                case CurrType.kind of
                  tkRecord:
                    begin
                      if CurrType is TRecordTypeDef then
                      begin
                        for i := 0 to TRecordTypeDef(CurrType).MemberCount - 1 do
                        begin
                          IdentStr := TRecordTypeDef(CurrType).GetMemberName(i);
                          MemberType := TRecordTypeDef(CurrType).GetMemberType(i);
                          AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CurrType, LastParserContext, CursorPChar);
                        end;
                      end;
                    end;
                  tkClass:
                    begin
                      CType := CurrType;
                      while CType <> nil do
                      begin
                        if (CType.kind = tkClass) and (CType is TClassTypeDef) then
                        begin
                          for i := 0 to TClassTypeDef(CType).MemberCount - 1 do
                          begin
                            IdentStr := TClassTypeDef(CType).GetMemberName(i);
                            MemberType := TClassTypeDef(CType).GetMemberType(i);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, CType, LastParserContext, CursorPChar);
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
                        if (OType.kind = tkObject) and (OType is TObjectTypeDef) then
                        begin
                          for i := 0 to TObjectTypeDef(OType).MemberCount - 1 do
                          begin
                            IdentStr := TObjectTypeDef(OType).GetMemberName(i);
                            MemberType := TObjectTypeDef(OType).GetMemberType(i);
                            AddCompletionItem(ItemsJson, AddedNames, IdentStr, MemberType, CurrentClassType, OType, LastParserContext, CursorPChar);
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
