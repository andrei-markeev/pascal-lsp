unit RecordSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, TypedToken;

type
    TRecordSpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    Anchors, Token, ReservedWord, VarDecl;

constructor TRecordSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
var
    i: integer;
    fieldDecl: TVarDecl;
    nextTokenKind: TTokenKind;
begin
    ctx.Add(Self);
    tokenName := 'RecordSpec';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDefToFill.size := 0;
    typeDefToFill.kind := tkRecord;
    typeDefToFill.fields := TFPHashList.Create; // TODO: free memory

    TReservedWord.Create(ctx, rwRecord, true);

    nextTokenKind := DetermineNextTokenKind(ctx);

    while (nextTokenKind.primitiveKind = pkIdentifier) or (nextTokenKind.reservedWordKind in [rwCase, rwEnd]) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            fieldDecl := TVarDecl.Create(ctx, parentSymbols);
            for i := 0 to length(fieldDecl.idents) - 1 do
            begin
                typeDefToFill.fields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType);
                inc(typeDefToFill.size, fieldDecl.varType.size);
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
