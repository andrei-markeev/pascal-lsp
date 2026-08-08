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
    typeSym: TSymbol;
    found: pointer;
begin
    if not PeekReservedWord(ctx, rwDot) then
    begin
        spec.state := tsError;
        spec.errorMessage := 'Type expected!';
        ctx.MarkEndOfToken(spec);
        exit(false);
    end;

    Result := true;
    TReservedWord.Create(ctx, rwDot, true);
    typeIdent := TIdentifier.Create(ctx, false);
    typeIdentName := typeIdent.GetStr();
    typeSym := FindUnitMemberSymbol(unitSym, typeIdentName, ctx.Cursor);

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
end;

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
var
    nextTokenKind: TTokenKind;
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
    found: pointer;
begin
    ctx.SkipTrivia;
    ctx.Add(Self);
    tokenName := 'TypeSpec';
    start := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);

    case nextTokenKind.primitiveKind of
        pkNumber, pkString:
            begin
                start := ctx.Cursor;
                TRangeSpec.Create(ctx, nextTokenKind, typeDefToFill);
                state := tsCorrect;
                ctx.MarkEndOfToken(Self);
                exit;
            end;
        pkIdentifier:
            begin
                start := ctx.Cursor;
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
                        start := ctx.Cursor;
                        TClassSpec.Create(ctx, parentSymbols, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwObject: ; // TODO: implement ObjectSpec
                rwRecord:
                    begin
                        start := ctx.Cursor;
                        TRecordSpec.Create(ctx, parentSymbols, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwSet:
                    begin
                        start := ctx.Cursor;
                        TSetSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwFile:
                    begin
                        start := ctx.Cursor;
                        TFileSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwString:
                    begin
                        start := ctx.Cursor;
                        TReservedWord.Create(ctx, rwString, true);
                        typeDefToFill := ansiString64Type;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwArray:
                    begin
                        start := ctx.Cursor;
                        TArraySpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwHat:
                    begin
                        start := ctx.Cursor;
                        TPointerSpec.Create(ctx, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwPlus, rwMinus:
                    begin
                        start := ctx.Cursor;
                        TRangeSpec.Create(ctx, nextTokenKind, typeDefToFill);
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwOpenParenthesis:
                    begin
                        start := ctx.Cursor;
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
