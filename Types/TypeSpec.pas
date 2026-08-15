unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, TypeDef, TypeDefs, Identifier;

type
    TTypeSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; ident: TIdentifier = nil);
    end;

function CreateTypeSpec(ctx: TParserContext; var typeDefToFill: TTypeDef): TTypeSpec;

implementation

uses
    CompilationMode, Anchors, ReservedWord, ConstValue,
    EnumSpec, RangeSpec, ArraySpec, SetSpec, RecordSpec, ClassSpec, PointerSpec, FileSpec;

function CreateTypeSpec(ctx: TParserContext; var typeDefToFill: TTypeDef): TTypeSpec;
begin
    CreateTypeSpec := TTypeSpec.Create(ctx, [nil], typeDefToFill);
end;

function FindUnitMemberSymbol(unitSym: TSymbol; const memberName: shortstring; cursor: PChar): TSymbol;
var
    i: integer;
begin
    Result := FindSymbol(unitSym, memberName, cursor);
    if (Result = nil) and (length(unitSym.children) > 0) then
    begin
        for i := 0 to length(unitSym.children) - 1 do
            if LowerCase(unitSym.children[i].displayName) = LowerCase(memberName) then
            begin
                Result := unitSym.children[i];
                break;
            end;
    end;
    if Result = nil then
        Result := FindSymbol(memberName, cursor);
end;

function ParseUnitQualifiedType(ctx: TParserContext; spec: TTypeSpec; unitSym: TSymbol; var typeDefToFill: TTypeDef): boolean;
var
    typeIdent: TIdentifier;
    typeIdentName: shortstring;
    typeSym, curUnitSym: TSymbol;
    found: pointer;
begin
    Result := true;
    curUnitSym := unitSym;
    while PeekReservedWord(ctx, rwDot) do
    begin
        TReservedWord.Create(ctx, rwDot, true);
        typeIdent := TIdentifier.Create(ctx, false);
        typeIdentName := typeIdent.GetStr();
        typeSym := FindUnitMemberSymbol(curUnitSym, typeIdentName, ctx.Cursor);

        if (typeSym <> nil) and (typeSym.kind = skUnitName) then
        begin
            curUnitSym := typeSym;
            typeSym.AddReference(typeIdent, true);
            continue;
        end;

        if (typeSym <> nil) and (typeSym.kind = skTypeName) then
        begin
            typeDefToFill := typeSym.typeDef;
            typeSym.AddReference(typeIdent, true);
            spec.state := tsCorrect;
            ctx.MarkEndOfToken(spec);
            exit;
        end
        else if typeSym = nil then
        begin
            found := TypesList.Find(LowerCase(typeIdentName));
            if found <> nil then
            begin
                typeDefToFill := TTypeDef(found);
                spec.state := tsCorrect;
                ctx.MarkEndOfToken(spec);
                exit;
            end;
        end;

        typeIdent.state := tsError;
        typeIdent.errorMessage := 'Type expected!';
        spec.state := tsError;
        spec.errorMessage := 'Type expected!';
        ctx.MarkEndOfToken(spec);
        exit(false);
    end;

    spec.state := tsError;
    spec.errorMessage := 'Type expected!';
    ctx.MarkEndOfToken(spec);
    Result := false;
end;

procedure ParseIdentifierTypeSpec(ctx: TParserContext; spec: TTypeSpec; var typeDefToFill: TTypeDef; ident: TIdentifier; const identName: shortstring);
var
    symbol: TSymbol;
    found: pointer;
begin
    symbol := FindSymbol(identName, spec.start);
    if symbol = nil then
    begin
        found := TypesList.Find(LowerCase(identName));
        if found = nil then
        begin
            if ident = nil then
                ident := TIdentifier.Create(ctx, false);
            spec.state := tsError;
            spec.errorMessage := 'Identifier has not been declared!';
            ctx.MarkEndOfToken(spec);
            exit;
        end;

        typeDefToFill := TTypeDef(found);
        if ident = nil then
            ident := TIdentifier.Create(ctx, false);
        if (typeDefToFill = shortstringType) and PeekReservedWord(ctx, rwOpenSquareBracket) then
        begin
            TReservedWord.Create(ctx, rwOpenSquareBracket, true);
            TConstValue.Create(ctx, DetermineNextTokenKind(ctx));
            TReservedWord.Create(ctx, rwCloseSquareBracket, false);
        end;
        spec.state := tsCorrect;
        ctx.MarkEndOfToken(spec);
        exit;
    end;

    case symbol.kind of
        skTypeName:
            begin
                typeDefToFill := symbol.typeDef;
                if ident = nil then
                    ident := TIdentifier.Create(ctx, false);
                symbol.AddReference(ident);
                if (typeDefToFill = shortstringType) and PeekReservedWord(ctx, rwOpenSquareBracket) then
                begin
                    TReservedWord.Create(ctx, rwOpenSquareBracket, true);
                    TConstValue.Create(ctx, DetermineNextTokenKind(ctx));
                    TReservedWord.Create(ctx, rwCloseSquareBracket, false);
                end;
                spec.state := tsCorrect;
                ctx.MarkEndOfToken(spec);
                exit;
            end;
        skUnitName:
            begin
                if ident = nil then
                    ident := TIdentifier.Create(ctx, false);
                symbol.AddReference(ident);
                ParseUnitQualifiedType(ctx, spec, symbol, typeDefToFill);
                exit;
            end;
        skConstant:
            begin
                if ident = nil then
                begin
                    TRangeSpec.Create(ctx, DetermineNextTokenKind(ctx), typeDefToFill);
                    spec.state := tsCorrect;
                end
                else
                begin
                    spec.state := tsError;
                    spec.errorMessage := 'Type expected!';
                end;
                ctx.MarkEndOfToken(spec);
                exit;
            end;
    end;

    if ident = nil then
        ident := TIdentifier.Create(ctx, false);
    symbol.AddReference(ident);
    spec.state := tsError;
    spec.errorMessage := 'Type expected!';
    ctx.MarkEndOfToken(spec);
end;

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; ident: TIdentifier = nil);
var
    nextTokenKind: TTokenKind;
    identName: shortstring;
    packedRW, pointerRW, partialRW: TReservedWord;
begin
    tokenName := 'TypeSpec';

    if ident <> nil then
    begin
        ctx.InsertBefore(ident, Self);
        start := ident.start;
        ParseIdentifierTypeSpec(ctx, Self, typeDefToFill, ident, ident.GetStr());
        exit;
    end;

    ctx.SkipTrivia;
    ctx.Add(Self);
    start := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);

    partialRW := nil;
    packedRW := nil;

    while nextTokenKind.reservedWordKind in [rwPartial, rwPacked, rwOptional] do
    begin
        if nextTokenKind.reservedWordKind = rwPartial then
            partialRW := TReservedWord.Create(ctx, rwPartial, true)
        else if nextTokenKind.reservedWordKind = rwOptional then
            TReservedWord.Create(ctx, rwOptional, true)
        else
            packedRW := TReservedWord.Create(ctx, rwPacked, true);
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    if (partialRW <> nil) and (nextTokenKind.reservedWordKind <> rwRecord) then
    begin
        partialRW.state := tsError;
        partialRW.errorMessage := 'Expected record after partial';
    end;

    if (packedRW <> nil) and not (nextTokenKind.reservedWordKind in [rwRecord, rwSet, rwArray]) then
    begin
        packedRW.state := tsError;
        packedRW.errorMessage := 'Expected record, set or array after packed';
    end;

    case nextTokenKind.primitiveKind of
        pkNumber, pkString:
            begin
                TRangeSpec.Create(ctx, nextTokenKind, typeDefToFill);
                state := tsCorrect;
                ctx.MarkEndOfToken(Self);
                exit;
            end;
        pkIdentifier:
            begin
                identName := PeekIdentifier(ctx);
                ParseIdentifierTypeSpec(ctx, Self, typeDefToFill, nil, identName);
                exit;
            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwClass:
                    begin
                        TClassSpec.Create(ctx, parentSymbols, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwObject: ; // TODO: implement ObjectSpec
                rwRecord:
                    begin
                        TRecordSpec.Create(ctx, parentSymbols, typeDefToFill, partialRW <> nil);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwSet:
                    begin
                        TSetSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwFile:
                    begin
                        TFileSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwString:
                    begin
                        TReservedWord.Create(ctx, rwString, true);
                        if mfAnsiStringDefault in Features[ctx.mode] then
                            typeDefToFill := ansiString64Type
                        else
                            typeDefToFill := shortstringType;

                        if PeekReservedWord(ctx, rwOpenSquareBracket) then
                        begin
                            TReservedWord.Create(ctx, rwOpenSquareBracket, true);
                            TConstValue.Create(ctx, DetermineNextTokenKind(ctx));
                            TReservedWord.Create(ctx, rwCloseSquareBracket, false);
                            typeDefToFill := shortstringType;
                        end;

                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwArray:
                    begin
                        TArraySpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwPointer:
                    begin
                        pointerRW := TReservedWord.Create(ctx, rwPointer, true);
                        if PeekReservedWord(ctx, rwTo) then
                        begin
                            TPointerSpec.Create(ctx, pointerRW, typeDefToFill);
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
                            exit;
                        end
                        else
                        begin
                            typeDefToFill := pointer64Type;
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
                            exit;
                        end;
                    end;
                rwHat:
                    begin
                        TPointerSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwPlus, rwMinus:
                    begin
                        TRangeSpec.Create(ctx, nextTokenKind, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwOpenParenthesis:
                    begin
                        TEnumSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
            end;
    end;

    state := tsMissing;
    start := ctx.GetCursorBeforeTrivia;
    len := 0;
end;

end.
