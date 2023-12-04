unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeRef in 'Types/TypeRef.pas', Token, ReservedWord, Identifier;

type
    TTypeKind = (tkUnknown, tkInteger, tkBoolean, tkChar, tkEnum, tkSubrange, tkReal, tkString, tkPointer, tkArray, tkRecord, tkObject, tkClass, tkSet, tkFile);

    TTypeSpec = class(TToken)
    public
        kind: TTypeKind;
        typeReference: TTypeRef;
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

    kind := tkUnknown;
    case nextTokenKind.primitiveKind of
        pkNumber: kind := tkSubrange;
        pkIdentifier:
            begin
                start := ctx.Cursor;
                typeName := PeekIdentifier(ctx);
                case LowerCase(typeName) of
                    { standard pascal predefined types }
                    'integer': kind := tkInteger;
                    'boolean': kind := tkBoolean;
                    'char': kind := tkChar;
                    'real': kind := tkReal;
                    { turbo pascal predefined types }
                    'shortint': kind := tkInteger; // TODO: sizes and ranges
                    'longint': kind := tkInteger;
                    'byte': kind := tkInteger;
                    'word': kind := tkInteger;
                    'bytebool': kind := tkBoolean;
                    'wordbool': kind := tkBoolean;
                    'longbool': kind := tkBoolean;
                    'single': kind := tkReal;
                    'double': kind := tkReal;
                    'extended': kind := tkReal;
                    'comp': kind := tkReal;
                    'pointer': kind := tkPointer;
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

                if kind <> tkUnknown then
                begin
                    typeToken := TIdentifier.Create(ctx);
                    state := tsCorrect;
                    ctx.MarkEndOfToken(Self);
                    exit;
                end;

            end;
        pkUnknown:
            case nextTokenKind.reservedWordKind of
                rwArray: kind := tkArray;
                rwClass: kind := tkClass;
                rwObject: kind := tkObject;
                rwRecord: kind := tkRecord;
                rwSet: kind := tkSet;
                rwFile: kind := tkFile;
                rwString: kind := tkString;
                rwPlus:
                    begin
                        TReservedWord.Create(ctx, rwPlus, true);
                        AddAnchor(pkIdentifier);
                        AddAnchor(pkNumber);
                        nextTokenKind := SkipUntilAnchor(ctx);
                        RemoveAnchor(pkIdentifier);
                        RemoveAnchor(pkNumber);
                        kind := tkSubrange;
                    end;
                rwMinus:
                    begin
                        TReservedWord.Create(ctx, rwPlus, true);
                        AddAnchor(pkIdentifier);
                        AddAnchor(pkNumber);
                        nextTokenKind := SkipUntilAnchor(ctx);
                        RemoveAnchor(pkIdentifier);
                        RemoveAnchor(pkNumber);
                        kind := tkSubrange;
                    end;
            end;
    end;

    if kind = tkUnknown then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    start := ctx.Cursor;
    state := tsCorrect;
    case kind of
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
