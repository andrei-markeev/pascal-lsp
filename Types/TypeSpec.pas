unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, Token, TypeDefs;

type
    TTypeSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
    end;

function CreateTypeSpec(ctx: TParserContext; var typeDefToFill: TTypeDef): TTypeSpec;

implementation

uses
    Anchors, ReservedWord, Identifier,
    EnumSpec, RangeSpec, ArraySpec, SetSpec, RecordSpec, ClassSpec;

function CreateTypeSpec(ctx: TParserContext; var typeDefToFill: TTypeDef): TTypeSpec;
begin
    CreateTypeSpec := TTypeSpec.Create(ctx, [nil], typeDefToFill);
end;

constructor TTypeSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
var
    nextTokenKind: TTokenKind;
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

                    typeDefToFill := PTypeDef(found)^;
                    TIdentifier.Create(ctx, false);
                    state := tsCorrect;
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;

                case symbol.kind of
                    skTypeName:
                        begin
                            typeDefToFill := symbol.typeDef^;
                            ident := TIdentifier.Create(ctx, false);
                            symbol.AddReference(ident);
                            state := tsCorrect;
                            ctx.MarkEndOfToken(Self);
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
                rwFile: ; // TODO: implement FileSpec
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
    len := 0;
end;

end.
