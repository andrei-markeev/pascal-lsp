unit InheritedRef;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef, VarRef;

procedure ParseInherited(ctx: TParserContext; ref: TVarRef);

implementation

uses
    Token, ReservedWord, Identifier, Symbols, Scopes, FunctionImpl, ClassTypeDef, ObjectTypeDef, Anchors;

function FindEnclosingFunctionName(ctx: TParserContext): shortstring;
var
    enclosingScope: TScope;
    enclosingFunc: TFunctionImpl;
begin
    Result := '';
    enclosingScope := FindScope(ctx.Cursor);
    while enclosingScope <> nil do
    begin
        if enclosingScope.funcImpl <> nil then
        begin
            enclosingFunc := TFunctionImpl(enclosingScope.funcImpl);
            if enclosingFunc.nameIdent <> nil then
                exit(enclosingFunc.nameIdent.GetStr());
        end;
        enclosingScope := enclosingScope.parentScope;
    end;
end;

procedure DetermineMethodTypes(ctx: TParserContext; out selfType, parentType: TTypeDef);
var
    selfSym: TSymbol;
begin
    selfType := nil;
    parentType := nil;
    selfSym := FindSymbol('Self', ctx.Cursor);
    if (selfSym <> nil) and (selfSym.typeDef <> nil) then
    begin
        selfType := selfSym.typeDef;
        if (selfType.kind = tkClass) and (selfType is TClassTypeDef) then
            parentType := TClassTypeDef(selfType).parentClass
        else if (selfType.kind = tkObject) and (selfType is TObjectTypeDef) then
            parentType := TObjectTypeDef(selfType).parentObject
        else
            parentType := nil;
    end;
end;

procedure ParseExplicitInherited(ctx: TParserContext; ref: TVarRef; selfType, parentType: TTypeDef);
var
    identName: shortstring;
    typeSym, foundSym: TSymbol;
    targetType: TTypeDef;
    typeIdent: TIdentifier;
    pCursor: PChar;
begin
    identName := PeekIdentifier(ctx);
    typeSym := FindSymbol(identName, ctx.Cursor);

    pCursor := ctx.Cursor + length(identName);
    while pCursor[0] in [#9, #10, #13, ' '] do inc(pCursor);

    if (typeSym <> nil) and (typeSym.kind = skTypeName) and (typeSym.typeDef <> nil) and (typeSym.typeDef.kind in [tkClass, tkObject]) and (pCursor[0] = '.') then
    begin
        targetType := typeSym.typeDef;
        typeIdent := TIdentifier.Create(ctx, true);
        if (selfType <> nil) and not IsSameOrSubclass(selfType, targetType) then
        begin
            typeIdent.state := tsError;
            typeIdent.errorMessage := identName + ' is not an ancestor of current class!';
            targetType := nil;
        end;
        TReservedWord.Create(ctx, rwDot, true);
        identName := PeekIdentifier(ctx);
    end
    else
        targetType := parentType;

    if targetType <> nil then
        foundSym := FindInheritedMemberSymbol(targetType, identName, ctx.Cursor)
    else
        foundSym := nil;

    if foundSym <> nil then
    begin
        ref.firstIdent := TIdentifier.Create(ctx, false);
        foundSym.AddReference(ref.firstIdent);
        ref.typeDef := foundSym.typeDef;
        ref.firstIdent.typeDef := foundSym.typeDef;
        if (ref.typeDef <> nil) and not IsMemberAccessible(ctx, targetType, ref.typeDef.visibility, ctx.Cursor, foundSym) then
        begin
            ref.firstIdent.state := tsError;
            ref.firstIdent.errorMessage := identName + ' is not public, it cannot be used here!';
        end;
    end
    else
    begin
        if (parentType = nil) and (selfType = nil) then
        begin
            ref.state := tsError;
            ref.errorMessage := 'Cannot use ''inherited'' outside of a method!';
        end;
        ref.firstIdent := TIdentifier.Create(ctx, true);
        ref.typeDef := ref.firstIdent.typeDef;
    end;
end;

procedure ParseImplicitInherited(ctx: TParserContext; ref: TVarRef; selfType, parentType: TTypeDef);
var
    enclosingName: shortstring;
    foundSym: TSymbol;
begin
    enclosingName := FindEnclosingFunctionName(ctx);

    if (parentType <> nil) and (enclosingName <> '') then
        foundSym := FindInheritedMemberSymbol(parentType, enclosingName, ctx.Cursor)
    else
        foundSym := nil;

    if foundSym <> nil then
    begin
        ref.typeDef := foundSym.typeDef;
        ref.firstIdent := TIdentifier.Create(ctx, false);
        ref.firstIdent.state := tsInvisible;
        foundSym.AddReference(ref.firstIdent);
        if (ref.typeDef <> nil) and not IsMemberAccessible(ctx, parentType, ref.typeDef.visibility, ctx.Cursor, foundSym) then
        begin
            ref.state := tsError;
            ref.errorMessage := enclosingName + ' is not public, it cannot be used here!';
        end;
    end
    else
    begin
        ref.state := tsError;
        if parentType = nil then
            ref.errorMessage := 'Cannot use ''inherited'' outside of a method!'
        else
            ref.errorMessage := 'No inherited method found to call!';
    end;
end;

procedure ParseInherited(ctx: TParserContext; ref: TVarRef);
var
    selfType, parentType: TTypeDef;
    nextTokenKind: TTokenKind;
begin
    ref.isSimple := false;
    TReservedWord.Create(ctx, rwInherited, true);

    DetermineMethodTypes(ctx, selfType, parentType);

    nextTokenKind := DetermineNextTokenKind(ctx);
    if nextTokenKind.primitiveKind = pkIdentifier then
        ParseExplicitInherited(ctx, ref, selfType, parentType)
    else
        ParseImplicitInherited(ctx, ref, selfType, parentType);
end;

end.
