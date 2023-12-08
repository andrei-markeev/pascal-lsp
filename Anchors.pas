unit Anchors;

{$mode objfpc}
{$longstrings on}

interface

uses
    CompilationMode, ParserContext, Token, InvalidSymbol, ReservedWord, Number, Identifier, StringToken;

const
    NUM_OF_PRIMITIVES = 4;

type
    TPrimitiveKind = (pkUnknown, pkNumber, pkString, pkIdentifier);
    TTokenKind = record
        reservedWordKind: TReservedWordKind;
        primitiveKind: TPrimitiveKind;
        isEOF: boolean;
    end;

procedure AddAnchor(kind: TPrimitiveKind);
procedure AddAnchor(kind: TReservedWordKind);
procedure RemoveAnchor(kind: TPrimitiveKind);
procedure RemoveAnchor(kind: TReservedWordKind);
function DetermineNextTokenKind(ctx: TParserContext): TTokenKind;
function SkipUntilAnchor(ctx: TParserContext): TTokenKind;

implementation

var
    PrimitiveKindAnchors: array [0..NUM_OF_PRIMITIVES - 1] of integer;
    ReservedWordAnchors: array [0..NUM_OF_RESERVED_WORDS - 1] of integer;

procedure AddAnchor(kind: TPrimitiveKind);
begin
    inc(PrimitiveKindAnchors[ord(kind)]);
end;

procedure RemoveAnchor(kind: TPrimitiveKind);
begin
    dec(PrimitiveKindAnchors[ord(kind)]);
end;

procedure AddAnchor(kind: TReservedWordKind);
begin
    inc(ReservedWordAnchors[ord(kind)]);
end;

procedure RemoveAnchor(kind: TReservedWordKind);
begin
    dec(ReservedWordAnchors[ord(kind)]);
end;

function DetermineNextTokenKind(ctx: TParserContext): TTokenKind;
begin
    with DetermineNextTokenKind do
    begin
        reservedWordKind := rwUnknown;
        primitiveKind := pkUnknown;
        isEOF := false;
        ctx.SkipTrivia;
        case ctx.Cursor[0] of
            '$': if (ctx.mode >= cmTurboPascal) and (ctx.Cursor[1] in ['0'..'9', 'a'..'z', 'A'..'Z']) then primitiveKind := pkNumber;
            '&': if (ctx.mode >= cmFreePascal) and (ctx.Cursor[1] in ['0'..'7']) then primitiveKind := pkNumber;
            '0'..'9': primitiveKind := pkNumber;
            '_': primitiveKind := pkIdentifier;
            '''', '#': primitiveKind := pkString;
            'A'..'Z', 'a'..'z', '+', '-', '*', '/', '^', '=', '<', '>', '(', ')', '[', ']', '{', '}', '.', ',', ':', ';', '@':
                begin
                    reservedWordKind := DetermineReservedWord(ctx);
                    if reservedWordKind = rwUnknown then
                        primitiveKind := pkIdentifier;
                end;
            #0: begin
                isEOF := true;
                exit;
            end;
        end;
    end;
end;

function SkipUntilAnchor(ctx: TParserContext): TTokenKind;
var
    skippedToken: TToken;
begin
    repeat
        SkipUntilAnchor := DetermineNextTokenKind(ctx);
        with SkipUntilAnchor do
        begin
            if reservedWordKind <> rwUnknown then
            begin
                if ReservedWordAnchors[ord(reservedWordKind)] > 0 then exit;
                skippedToken := TReservedWord.Create(ctx, reservedWordKind, true);
                skippedToken.state := tsSkipped;
            end
            else if primitiveKind <> pkUnknown then
            begin
                if PrimitiveKindAnchors[ord(primitiveKind)] > 0 then exit
                else
                begin
                    case primitiveKind of
                        pkNumber: skippedToken := TNumber.Create(ctx);
                        pkString: skippedToken := TStringToken.Create(ctx);
                        pkIdentifier: skippedToken := TIdentifier.Create(ctx);
                    end;
                    skippedToken.state := tsSkipped;
                end;
            end
            else
                TInvalidSymbol.Create(ctx);
        end;
    until ctx.IsEOF;

end;

end.
