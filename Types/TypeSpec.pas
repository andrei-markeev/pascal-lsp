unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier, EnumSpec, RangeSpec;

type
    TTypeSpec = class(TToken)
    public
        typeDef: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TTypeSpec.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    rangeSpecToken: TRangeSpec;
    enumSpecToken: TEnumSpec;
    ident: TIdentifier;
    identName: shortstring;
    symbol: TSymbol;
    found: pointer;
begin
    ctx.Add(Self);
    tokenName := 'TypeSpec';
    start := ctx.Cursor;

    AddAnchor(pkNumber);
    AddAnchor(pkIdentifier);
    AddAnchor(pkString);
    AddAnchor(rwArray);
    AddAnchor(rwClass);
    AddAnchor(rwObject);
    AddAnchor(rwRecord);
    AddAnchor(rwSet);
    AddAnchor(rwFile);
    AddAnchor(rwString);
    AddAnchor(rwMinus);
    AddAnchor(rwPlus);
    AddAnchor(rwOpenParenthesis);

    nextTokenKind := SkipUntilAnchor(ctx);

    RemoveAnchor(pkNumber);
    RemoveAnchor(pkIdentifier);
    RemoveAnchor(pkString);
    RemoveAnchor(rwArray);
    RemoveAnchor(rwClass);
    RemoveAnchor(rwObject);
    RemoveAnchor(rwRecord);
    RemoveAnchor(rwSet);
    RemoveAnchor(rwFile);
    RemoveAnchor(rwString);
    RemoveAnchor(rwMinus);
    RemoveAnchor(rwPlus);
    RemoveAnchor(rwOpenParenthesis);

    typeDef.kind := tkUnknown;
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
                symbol := FindSymbol(identName);
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
                rwArray: typeDef.kind := tkArray;
                rwClass: typeDef.kind := tkClass;
                rwObject: typeDef.kind := tkObject;
                rwRecord: typeDef.kind := tkRecord;
                rwSet: typeDef.kind := tkSet;
                rwFile: typeDef.kind := tkFile;
                rwString: typeDef.kind := tkString;
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

    if typeDef.kind = tkUnknown then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    start := ctx.Cursor;
    state := tsCorrect;
    case typeDef.kind of
        tkString: TReservedWord.Create(ctx, rwString, true);
    else
        // TODO: implement more types
        WriteLn('Not implemented!');
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
