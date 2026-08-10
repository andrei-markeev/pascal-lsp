unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, TypeDef, TypeDefs, Identifier;

type
    TTypeSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef); overload;
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; ident: TIdentifier); overload;
    end;

function CreateTypeSpec(ctx: TParserContext; var typeDefToFill: TTypeDef): TTypeSpec;

implementation

uses
    Anchors, ReservedWord,
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
            typeSym.AddReference(typeIdent);
            continue;
        end;

        if (typeSym <> nil) and (typeSym.kind = skTypeName) then
        begin
            typeDefToFill := typeSym.typeDef;
            typeSym.AddReference(typeIdent);
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

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
var
    nextTokenKind: TTokenKind;
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
    found: pointer;
    packedRW: TReservedWord;
begin
    ctx.SkipTrivia;
    ctx.Add(Self);
    tokenName := 'TypeSpec';
    start := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);

    if nextTokenKind.reservedWordKind = rwPacked then
    begin
        packedRW := TReservedWord.Create(ctx, rwPacked, true);
        nextTokenKind := DetermineNextTokenKind(ctx);
        if not (nextTokenKind.reservedWordKind in [rwRecord, rwSet, rwArray]) then
        begin
            packedRW.state := tsError;
            packedRW.errorMessage := 'Expected record, set or array after packed';
            nextTokenKind := DetermineNextTokenKind(ctx);
        end;
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
                symbol := FindSymbol(identName, ctx.Cursor);
                if symbol = nil then
                begin
                    found := TypesList.Find(LowerCase(identName));
                    if found = nil then
                    begin
                        TIdentifier.Create(ctx, false);
                        state := tsError;
                        errorMessage := 'Identifier has not been declared!';
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;

                    typeDefToFill := TTypeDef(found);
                    TIdentifier.Create(ctx, false);
                    state := tsCorrect;
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;

                case symbol.kind of
                    skTypeName:
                        begin
                            typeDefToFill := symbol.typeDef;
                            ident := TIdentifier.Create(ctx, false);
                            symbol.AddReference(ident);
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
                            exit;
                        end;
                    skUnitName:
                        begin
                            ident := TIdentifier.Create(ctx, false);
                            symbol.AddReference(ident);
                            ParseUnitQualifiedType(ctx, Self, symbol, typeDefToFill);
                            exit;
                        end;
                    skConstant:
                        begin
                            start := ctx.Cursor;
                            TRangeSpec.Create(ctx, nextTokenKind, typeDefToFill);
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
                            exit;
                        end;
                end;

                ident := TIdentifier.Create(ctx, false);
                symbol.AddReference(ident);
                state := tsError;
                errorMessage := 'Type expected!';
                ctx.MarkEndOfToken(Self);
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
                        TRecordSpec.Create(ctx, parentSymbols, typeDefToFill);
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
                        typeDefToFill := ansiString64Type;
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

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; ident: TIdentifier);
var
    identName: shortstring;
    symbol: TSymbol;
    found: pointer;
begin
    ctx.InsertBefore(ident, Self);
    tokenName := 'TypeSpec';
    start := ident.start;

    identName := ident.GetStr();
    symbol := FindSymbol(identName, ident.start);
    if symbol = nil then
    begin
        found := TypesList.Find(LowerCase(identName));
        if found = nil then
        begin
            state := tsError;
            errorMessage := 'Identifier has not been declared!';
            ctx.MarkEndOfToken(Self);
            exit;
        end;

        typeDefToFill := TTypeDef(found);
        state := tsCorrect;
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    case symbol.kind of
        skTypeName:
            begin
                typeDefToFill := symbol.typeDef;
                symbol.AddReference(ident);
                state := tsCorrect;
                ctx.MarkEndOfToken(Self);
                exit;
            end;
        skUnitName:
            begin
                symbol.AddReference(ident);
                ParseUnitQualifiedType(ctx, Self, symbol, typeDefToFill);
                exit;
            end;
        skConstant:
            begin
                state := tsError;
                errorMessage := 'Type expected!';
                ctx.MarkEndOfToken(Self);
                exit;
            end;
    end;

    symbol.AddReference(ident);
    state := tsError;
    errorMessage := 'Type expected!';
    ctx.MarkEndOfToken(Self);
end;

end.
