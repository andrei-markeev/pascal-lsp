unit MemberAccess;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken, TypeDef, ReservedWord, VarRef;

function FindMemberInType(targetType: TTypeDef; const memberName: string; out declaringType: TTypeDef): pointer;
procedure ParseDotAccess(ctx: TParserContext; ref: TVarRef);

implementation

uses
    sysutils, Token, Identifier, TypeDefs, Symbols, ClassTypeDef, RecordTypeDef, ObjectTypeDef;

function FindMemberInType(targetType: TTypeDef; const memberName: string; out declaringType: TTypeDef): pointer;
var
    curType: TTypeDef;
    found: pointer;
begin
    declaringType := nil;
    curType := targetType;
    while curType <> nil do
    begin
        found := nil;
        case curType.kind of
            tkRecord:
                begin
                    if curType is TRecordTypeDef then
                        found := TRecordTypeDef(curType).FindMember(memberName);
                    if found <> nil then
                    begin
                        declaringType := curType;
                        exit(found);
                    end;
                    break;
                end;
            tkObject:
                begin
                    if curType is TObjectTypeDef then
                    begin
                        found := TObjectTypeDef(curType).FindMember(memberName);
                        if found <> nil then
                        begin
                            declaringType := curType;
                            exit(found);
                        end;
                        curType := TObjectTypeDef(curType).parentObject;
                    end
                    else
                        break;
                end;
            tkClass:
                begin
                    if curType is TClassTypeDef then
                    begin
                        found := TClassTypeDef(curType).FindMember(memberName);
                        if found <> nil then
                        begin
                            declaringType := curType;
                            exit(found);
                        end;
                        curType := TClassTypeDef(curType).parentClass;
                    end
                    else
                        break;
                end;
        else
            curType := nil;
        end;
    end;
    Result := nil;
end;

procedure ParseUnitDotAccess(ctx: TParserContext; ref: TVarRef);
var
    ident: TIdentifier;
    text: string;
    foundSym: TSymbol;
    i: integer;
begin
    ident := TIdentifier.Create(ctx, false);
    text := ident.GetStr();
    foundSym := FindSymbol(ref.symbol, text, ctx.Cursor);
    if (foundSym = nil) and (length(ref.symbol.children) > 0) then
    begin
        for i := 0 to length(ref.symbol.children) - 1 do
            if LowerCase(ref.symbol.children[i].displayName) = LowerCase(text) then
            begin
                foundSym := ref.symbol.children[i];
                break;
            end;
    end;
    if foundSym = nil then
        foundSym := FindSymbol(text, ctx.Cursor);

    ref.symbol := foundSym;
    if ref.symbol <> nil then
    begin
        ref.symbol.AddReference(ident);
        ref.typeDef := ref.symbol.typeDef;
    end
    else
    begin
        ident.state := tsError;
        ident.errorMessage := 'Identifier ''' + text + ''' was not found in unit ''' + ref.firstIdent.GetStr() + '''!';
        ref.typeDef := unknownType;
    end;
    ref.isSimple := false;
end;

procedure ParseMemberDotAccess(ctx: TParserContext; ref: TVarRef; reservedWordToken: TReservedWord);
var
    ident: TIdentifier;
    text: string;
    curType: TTypeDef;
    found: pointer;
begin
    if (ref.typeDef = nil) or not (ref.typeDef.kind in [tkRecord, tkClass, tkObject]) then
    begin
        SetString(text, ref.start, ctx.Cursor - ref.start - 1);
        reservedWordToken.state := tsError;
        reservedWordToken.errorMessage := 'Cannot apply ''.'' on ' + text + ' because it is not of a structured type (record, class or object)!';
    end;

    ident := TIdentifier.Create(ctx, false);
    ref.isSimple := false;

    if (ref.typeDef = nil) or not (ref.typeDef.kind in [tkRecord, tkClass, tkObject]) then
        exit;

    text := ident.GetStr();
    found := FindMemberInType(ref.typeDef, text, curType);

    if found = nil then
    begin
        ident.state := tsError;
        ident.errorMessage := 'Field or method with the name ''' + text + ''' was not found!';
        ref.typeDef := unknownType;
        exit;
    end;

    ref.typeDef := TTypeDef(found);

    if (curType <> nil) and (curType.typeSymbol <> nil) then
    begin
        ref.symbol := FindSymbol(TSymbol(curType.typeSymbol), text, ctx.Cursor);
        if ref.symbol <> nil then
            ref.symbol.AddReference(ident);
    end;

    if (ref.typeDef <> nil) and not IsMemberAccessible(ctx, curType, ref.typeDef.visibility, ctx.Cursor, ref.symbol) then
    begin
        ident.state := tsError;
        ident.errorMessage := text + ' is not public, it cannot be used here!';
    end;
end;

procedure ParseDotAccess(ctx: TParserContext; ref: TVarRef);
var
    reservedWordToken: TReservedWord;
begin
    reservedWordToken := TReservedWord.Create(ctx, rwDot, true);

    if (ref.symbol <> nil) and (ref.symbol.kind = skUnitName) then
        ParseUnitDotAccess(ctx, ref)
    else
        ParseMemberDotAccess(ctx, ref, reservedWordToken);
end;

end.
