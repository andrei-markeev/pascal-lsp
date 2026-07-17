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
    Anchors, Token, ReservedWord, VarDecl, Identifier, Number, StringToken, TypeSpec, CompilationMode, CaseBranch;



procedure ParseFields(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; endKind: TReservedWordKind);
var
    i, p: integer;
    fieldDecl: TVarDecl;
    nextTokenKind: TTokenKind;
    tagIdent: TIdentifier;
    tagType: TTypeDef;
    tagSymbols: array of TSymbol;
    dummySymbols: array of TSymbol;
    rangeRW: TReservedWord;
begin
    nextTokenKind := DetermineNextTokenKind(ctx);
    while not nextTokenKind.isEOF and (nextTokenKind.reservedWordKind <> endKind) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            fieldDecl := TVarDecl.Create(ctx, parentSymbols);
            for i := 0 to length(fieldDecl.idents) - 1 do
            begin
                typeDefToFill.recordFields.Add(fieldDecl.idents[i].GetStr(), @fieldDecl.varType);
                inc(typeDefToFill.size, fieldDecl.varType.size);
            end;
            nextTokenKind := DetermineNextTokenKind(ctx);
            if nextTokenKind.reservedWordKind in [endKind, rwEnd] then
            begin
                if PeekReservedWord(ctx, rwSemiColon) then
                    TReservedWord.Create(ctx, rwSemiColon, true);
            end
            else
                TReservedWord.Create(ctx, rwSemiColon, false);
        end
        else if nextTokenKind.reservedWordKind = rwCase then
        begin
            TReservedWord.Create(ctx, rwCase, true);
            
            nextTokenKind := DetermineNextTokenKind(ctx);
            if nextTokenKind.primitiveKind = pkIdentifier then
            begin
                tagIdent := TIdentifier.Create(ctx, false);
                if PeekReservedWord(ctx, rwColon) then
                begin
                    TReservedWord.Create(ctx, rwColon, true);
                    
                    SetLength(tagSymbols, length(parentSymbols));
                    for p := 0 to length(parentSymbols) - 1 do
                        tagSymbols[p] := RegisterSymbol(tagIdent, parentSymbols[p], skVariable, @tagType, ctx.Cursor);
                    
                    TTypeSpec.Create(ctx, tagSymbols, tagType);
                    typeDefToFill.recordFields.Add(tagIdent.GetStr(), @tagType);
                    inc(typeDefToFill.size, tagType.size);
                end
                else
                begin
                    SetLength(dummySymbols, 0);
                    TTypeSpec.Create(ctx, dummySymbols, tagType, tagIdent);
                end;
            end
            else
            begin
                SetLength(dummySymbols, 0);
                TTypeSpec.Create(ctx, dummySymbols, tagType);
            end;
            
            TReservedWord.Create(ctx, rwOf, false);
            
            nextTokenKind := DetermineNextTokenKind(ctx);
            while (nextTokenKind.primitiveKind in [pkNumber, pkString, pkIdentifier]) or (nextTokenKind.reservedWordKind = rwMinus) do
            begin
                ParseCaseConstant(ctx);
                if PeekReservedWord(ctx, rwRange) then
                begin
                    rangeRW := TReservedWord.Create(ctx, rwRange, true);
                    if ctx.mode = cmStandardPascal then
                    begin
                        rangeRW.state := tsError;
                        rangeRW.errorMessage := '".." ranges in case statements not supported in Standard Pascal (ISO 7185)';
                    end;
                    ParseCaseConstant(ctx);
                end;
                
                while PeekReservedWord(ctx, rwComma) do
                begin
                    TReservedWord.Create(ctx, rwComma, true);
                    ParseCaseConstant(ctx);
                    if PeekReservedWord(ctx, rwRange) then
                    begin
                        rangeRW := TReservedWord.Create(ctx, rwRange, true);
                        if ctx.mode = cmStandardPascal then
                        begin
                            rangeRW.state := tsError;
                            rangeRW.errorMessage := '".." ranges in case statements not supported in Standard Pascal (ISO 7185)';
                        end;
                        ParseCaseConstant(ctx);
                    end;
                end;
                
                TReservedWord.Create(ctx, rwColon, false);
                TReservedWord.Create(ctx, rwOpenParenthesis, false);
                
                ParseFields(ctx, parentSymbols, typeDefToFill, rwCloseParenthesis);
                
                TReservedWord.Create(ctx, rwCloseParenthesis, false);
                
                if PeekReservedWord(ctx, rwSemiColon) then
                    TReservedWord.Create(ctx, rwSemiColon, true);
                
                nextTokenKind := DetermineNextTokenKind(ctx);
            end;
            break;
        end
        else
            break;
            
        nextTokenKind := DetermineNextTokenKind(ctx);
    end;
end;

constructor TRecordSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
begin
    ctx.Add(Self);
    tokenName := 'RecordSpec';
    start := ctx.Cursor;
    state := tsCorrect;
    typeDefToFill.size := 0;
    typeDefToFill.kind := tkRecord;
    typeDefToFill.recordFields := TFPHashList.Create; // TODO: free memory

    TReservedWord.Create(ctx, rwRecord, true);

    ParseFields(ctx, parentSymbols, typeDefToFill, rwEnd);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
