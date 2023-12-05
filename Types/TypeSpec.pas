unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier;

type
    TTypeSpec = class(TToken)
    public
        typeDef: TTypeDef;
        typeToken: TToken;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TTypeSpec.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    symbol: TSymbol;
    typeName: shortstring;
begin
    ctx.Add(Self);
    tokenName := 'TypeSpec';
    start := ctx.Cursor;

    AddAnchor(pkNumber);
    AddAnchor(pkIdentifier);
    AddAnchor(rwArray);
    AddAnchor(rwClass);
    AddAnchor(rwObject);
    AddAnchor(rwRecord);
    AddAnchor(rwSet);
    AddAnchor(rwFile);
    AddAnchor(rwString);
    AddAnchor(rwMinus);
    AddAnchor(rwPlus);

    nextTokenKind := SkipUntilAnchor(ctx);

    RemoveAnchor(pkNumber);
    RemoveAnchor(pkIdentifier);
    RemoveAnchor(rwArray);
    RemoveAnchor(rwClass);
    RemoveAnchor(rwObject);
    RemoveAnchor(rwRecord);
    RemoveAnchor(rwSet);
    RemoveAnchor(rwFile);
    RemoveAnchor(rwString);
    RemoveAnchor(rwMinus);
    RemoveAnchor(rwPlus);

    typeDef.kind := tkUnknown;
    case nextTokenKind.primitiveKind of
        pkNumber: typeDef.kind := tkInteger;
        pkIdentifier:
            begin
                start := ctx.Cursor;
                typeName := PeekIdentifier(ctx);
                case LowerCase(typeName) of
                    { standard pascal predefined types }
                    'integer': typeDef.kind := tkInteger;
                    'boolean': typeDef.kind := tkBoolean;
                    'char': typeDef.kind := tkChar;
                    'real': typeDef.kind := tkReal;
                    { turbo pascal predefined types }
                    'shortint': typeDef.kind := tkInteger; // TODO: sizes and ranges
                    'longint': typeDef.kind := tkInteger;
                    'byte': typeDef.kind := tkInteger;
                    'word': typeDef.kind := tkInteger;
                    'bytebool': typeDef.kind := tkBoolean;
                    'wordbool': typeDef.kind := tkBoolean;
                    'longbool': typeDef.kind := tkBoolean;
                    'single': typeDef.kind := tkReal;
                    'double': typeDef.kind := tkReal;
                    'extended': typeDef.kind := tkReal;
                    'comp': typeDef.kind := tkReal;
                    'pointer': typeDef.kind := tkPointer;
                    { object pascal predefined types }
                    // TODO
                else
                    symbol := FindSymbol(typeName);

                    if symbol = nil then
                    begin
                        typeToken := TIdentifier.Create(ctx);
                        state := tsError;
                        errorMessage := 'Type identifier is undefined!';
                        ctx.MarkEndOfToken(Self);
                        exit;
                    end;

                    // TODO
                    // if symbol is of type skTypeName then assign type from symbol, add reference, etc.
                    // if symbol is of type skConstant and is integer, then it's a subrange
                    // if symbol is of type 'ordinal identifier', then it's a subrange

                end;

                if typeDef.kind <> tkUnknown then
                begin
                    typeToken := TIdentifier.Create(ctx);
                    state := tsCorrect;
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;

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
                rwPlus:
                    begin
                        TReservedWord.Create(ctx, rwPlus, true);
                        AddAnchor(pkIdentifier);
                        AddAnchor(pkNumber);
                        nextTokenKind := SkipUntilAnchor(ctx);
                        RemoveAnchor(pkIdentifier);
                        RemoveAnchor(pkNumber);
                        typeDef.kind := tkInteger;
                        // TODO: start end range
                    end;
                rwMinus:
                    begin
                        TReservedWord.Create(ctx, rwPlus, true);
                        AddAnchor(pkIdentifier);
                        AddAnchor(pkNumber);
                        nextTokenKind := SkipUntilAnchor(ctx);
                        RemoveAnchor(pkIdentifier);
                        RemoveAnchor(pkNumber);
                        typeDef.kind := tkInteger;
                        // TODO: start end range
                    end;
                rwOpenParenthesis:
                    begin
                        TReservedWord.Create(ctx, rwOpenParenthesis, true);
                        typeDef.kind := tkEnum;
                        // TODO: parse enum identifiers
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
        WriteLn('Not implemented!');
    end;

    ctx.MarkEndOfToken(Self);
end;

destructor TTypeSpec.Destroy;
begin
end;

end.
