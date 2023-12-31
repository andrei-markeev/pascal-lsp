unit RecordSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, TypedToken;

type
    TRecordSpec = class(TTypedToken)
    public
        fields: TFPHashList;
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol);
    end;

implementation

uses
    Anchors, Token, ReservedWord, TypeSpec, VarDecl;

constructor TRecordSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    i: integer;
    fieldDecl: TVarDecl;
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'RecordSpec';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDef.size := 0;
    typeDef.kind := tkRecord;
    fields := TFPHashList.Create;

    TReservedWord.Create(ctx, rwRecord, true);

    nextTokenKind := DetermineNextTokenKind(ctx);

    while (nextTokenKind.primitiveKind = pkIdentifier) or (nextTokenKind.reservedWordKind in [rwCase, rwEnd]) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            fieldDecl := TVarDecl.Create(ctx, parentSymbols);
            for i := 0 to length(fieldDecl.idents) - 1 do
            begin
                fields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType.typeDef);
                inc(typeDef.size, fieldDecl.varType.typeDef.size);
            end;
            TReservedWord.Create(ctx, rwSemiColon, false);
        end
        else if nextTokenKind.reservedWordKind = rwCase then
        begin
            TReservedWord.Create(ctx, rwCase, true);
            // TODO: handle variable part
        end
        else
        begin
            TReservedWord.Create(ctx, rwEnd, true);
            break;
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
