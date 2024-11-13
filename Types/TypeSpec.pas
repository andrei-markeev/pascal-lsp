unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, TypedToken, TypeDefs;

type
    TTypeSpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol);
    end;

function CreateTypeSpec(ctx: TParserContext): TTypeSpec;

implementation

uses
    Anchors, Token, ReservedWord, Identifier,
    EnumSpec, RangeSpec, ArraySpec, SetSpec, RecordSpec, ClassSpec;

function CreateTypeSpec(ctx: TParserContext): TTypeSpec;
begin
    CreateTypeSpec := TTypeSpec.Create(ctx, [nil]);
end;

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    nextTokenKind: TTokenKind;
    rangeSpecToken: TRangeSpec;
    enumSpecToken: TEnumSpec;
    arraySpecToken: TArraySpec;
    setSpecToken: TSetSpec;
    recordSpecToken: TRecordSpec;
    classSpecToken: TClassSpec;
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
    found: pointer;
begin
    ctx.Add(Self);
    tokenName := 'TypeSpec';
    start := ctx.Cursor;

    nextTokenKind := DetermineNextTokenKind(ctx);

    case nextTokenKind.primitiveKind of
        pkNumber, pkString:
            begin
                start := ctx.Cursor;
                rangeSpecToken := TRangeSpec.Create(ctx, nextTokenKind);
                typeDef := rangeSpecToken.typeDef;
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
                    found := TypesList.Find(identName);
                    if found = nil then
                    begin
                        TIdentifier.Create(ctx, false);
                        state := tsError;
                        errorMessage := 'Identifier has not been declared!';
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;

                    typeDef := PTypeDef(found)^;
                    TIdentifier.Create(ctx, false);
                    state := tsCorrect;
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;

                case symbol.kind of
                    skTypeName:
                        begin
                            typeDef := symbol.typeDef;
                            ident := TIdentifier.Create(ctx, false);
                            symbol.AddReference(ident);
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
                            exit;
                        end;
                    skConstant:
                        begin
                            start := ctx.Cursor;
                            rangeSpecToken := TRangeSpec.Create(ctx, nextTokenKind);
                            typeDef := rangeSpecToken.typeDef;
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
                        classSpecToken := TClassSpec.Create(ctx, parentSymbols);
                        typeDef := classSpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwObject: typeDef.kind := tkObject; // TODO: implement ObjectSpec
                rwRecord:
                    begin
                        start := ctx.Cursor;
                        recordSpecToken := TRecordSpec.Create(ctx, parentSymbols);
                        typeDef := recordSpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwSet:
                    begin
                        start := ctx.Cursor;
                        setSpecToken := TSetSpec.Create(ctx);
                        typeDef := setSpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwFile: typeDef.kind := tkFile; // TODO: implement FileSpec
                rwString:
                    begin
                        start := ctx.Cursor;
                        TReservedWord.Create(ctx, rwString, true);
                        typeDef.kind := tkString;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwArray:
                    begin
                        start := ctx.Cursor;
                        arraySpecToken := TArraySpec.Create(ctx);
                        typeDef := arraySpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwPlus, rwMinus:
                    begin
                        start := ctx.Cursor;
                        rangeSpecToken := TRangeSpec.Create(ctx, nextTokenKind);
                        typeDef := rangeSpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
                rwOpenParenthesis:
                    begin
                        start := ctx.Cursor;
                        enumSpecToken := TEnumSpec.Create(ctx);
                        typeDef := enumSpecToken.typeDef;
                        state := tsCorrect;
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;
            end;
    end;

    typeDef := default(TTypeDef);
    state := tsMissing;
    len := 0;
end;

end.
