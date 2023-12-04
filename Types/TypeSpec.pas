unit TypeSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord;

type
    TTypeKind = (tkUnknown, tkInteger, tkBoolean, tkChar, tkEnum, tkSubrange, tkReal, tkString, tkArray, tkRecord, tkObject, tkClass, tkSet, tkFile);

    TTypeSpec = class(TToken)
    public
        kind: TTypeKind;
        typeToken: TToken;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TTypeSpec.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
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
                // get identifier
                // get the symbol
                // if symbol is of type skTypeName then assign type from symbol, add reference, etc.
                // if symbol is of type skConstant and is integer, then it's a subrange
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
            end;
    end;

    if kind = tkUnknown then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    state := tsCorrect;
    len := ctx.Cursor - start;
end;

destructor TTypeSpec.Destroy;
begin
end;

end.
