unit RecordSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDef, TypeDefs, TypedToken;

type
    TRecordSpec = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; isPartial: boolean = false);
    end;

implementation

uses
    Anchors, Token, ReservedWord, VarDecl, Identifier, Number, StringToken, TypeSpec, CompilationMode, CaseBranch, RecordTypeDef, BranchTracker;

procedure ParseFields(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; endKind: TReservedWordKind; activeTagSymbol: TSymbol = nil; const activeLabels: TCaseLabelArray = nil);
var
    i, p, l: integer;
    cursor1, cursor2, cursor3: PChar;
    fieldDecl: TVarDecl;
    nextTokenKind: TTokenKind;
    tagIdent: TIdentifier;
    tagType: TTypeDef;
    tagSymbols: array of TSymbol;
    dummySymbols: array of TSymbol;
    rangeRW, caseRW: TReservedWord;
    recTypeDef: TRecordTypeDef;
    currentTagSymbol: TSymbol;
    branchLabels: TCaseLabelArray;
begin
    recTypeDef := TRecordTypeDef(typeDefToFill);
    nextTokenKind := DetermineNextTokenKind(ctx);
    while not nextTokenKind.isEOF and (nextTokenKind.reservedWordKind <> endKind) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            fieldDecl := TVarDecl.Create(ctx, parentSymbols);
            for i := 0 to length(fieldDecl.idents) - 1 do
            begin
                if recTypeDef.FindMember(fieldDecl.idents[i].GetStr()) <> nil then
                begin
                    fieldDecl.idents[i].state := tsError;
                    fieldDecl.idents[i].errorMessage := 'Field ''' + fieldDecl.idents[i].GetStr() + ''' already defined in record!';
                end
                else
                begin
                    recTypeDef.AddMember(fieldDecl.idents[i].GetStr(), fieldDecl.varType);
                    if activeTagSymbol <> nil then
                        recTypeDef.AddVariantFieldInfo(TSymbol(fieldDecl.idents[i].symbol), activeTagSymbol, activeLabels);
                    if fieldDecl.varType <> nil then
                        inc(recTypeDef.size, fieldDecl.varType.size);
                end;
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
            caseRW := TReservedWord.Create(ctx, rwCase, true);
            currentTagSymbol := nil;
            
            tagType := unknownType;
            nextTokenKind := DetermineNextTokenKind(ctx);
            if nextTokenKind.primitiveKind = pkIdentifier then
            begin
                tagIdent := TIdentifier.Create(ctx, false);
                if PeekReservedWord(ctx, rwColon) then
                begin
                    TReservedWord.Create(ctx, rwColon, true);
                    
                    SetLength(tagSymbols, length(parentSymbols));
                    for p := 0 to length(parentSymbols) - 1 do
                        tagSymbols[p] := RegisterSymbol(tagIdent, parentSymbols[p], skVariable, tagType, ctx.Cursor);
                    if length(tagSymbols) > 0 then
                        currentTagSymbol := tagSymbols[0];
                    
                    TTypeSpec.Create(ctx, tagSymbols, tagType);
                    if recTypeDef.FindMember(tagIdent.GetStr()) <> nil then
                    begin
                        tagIdent.state := tsError;
                        tagIdent.errorMessage := 'Field ''' + tagIdent.GetStr() + ''' already defined in record!';
                    end
                    else
                    begin
                        recTypeDef.AddMember(tagIdent.GetStr(), tagType);
                        if tagType <> nil then
                            inc(recTypeDef.size, tagType.size);
                    end;
                end
                else
                begin
                    SetLength(dummySymbols, 0);
                    TTypeSpec.Create(ctx, dummySymbols, tagType, tagIdent);
                    if mfStrictVariantRecords in Features[ctx.mode] then
                    begin
                        caseRW.state := tsError;
                        caseRW.errorMessage := 'Anonymous variant tag is not allowed! Please use "case <field>: <Type> of".';
                    end;
                end;
            end
            else
            begin
                SetLength(dummySymbols, 0);
                TTypeSpec.Create(ctx, dummySymbols, tagType);
                if mfStrictVariantRecords in Features[ctx.mode] then
                begin
                    caseRW.state := tsError;
                    caseRW.errorMessage := 'Anonymous variant tag is not allowed! Please use "case <field>: <Type> of".';
                end;
            end;
            
            TReservedWord.Create(ctx, rwOf, false);
            
            nextTokenKind := DetermineNextTokenKind(ctx);
            while (nextTokenKind.primitiveKind in [pkNumber, pkString, pkIdentifier]) or (nextTokenKind.reservedWordKind = rwMinus) do
            begin
                SetLength(branchLabels, 0);
                repeat
                    if (Length(branchLabels) > 0) and PeekReservedWord(ctx, rwComma) then
                        TReservedWord.Create(ctx, rwComma, true);

                    ctx.SkipTrivia;
                    cursor1 := ctx.Cursor;
                    ParseCaseConstant(ctx);
                    cursor2 := ctx.Cursor;

                    l := Length(branchLabels);
                    SetLength(branchLabels, l + 1);

                    if PeekReservedWord(ctx, rwRange) then
                    begin
                        rangeRW := TReservedWord.Create(ctx, rwRange, true);
                        if not (mfCaseRanges in Features[ctx.mode]) then
                        begin
                            rangeRW.state := tsError;
                            rangeRW.errorMessage := '".." ranges in case statements not supported in this compilation mode.';
                        end;
                        ctx.SkipTrivia;
                        cursor3 := ctx.Cursor;
                        ParseCaseConstant(ctx);
                        branchLabels[l] := CreateRangeLabel(cursor1, cursor2 - cursor1, cursor3, ctx.Cursor - cursor3);
                    end
                    else
                        branchLabels[l] := CreateSingleLabel(cursor1, cursor2 - cursor1);

                until not PeekReservedWord(ctx, rwComma);
                
                TReservedWord.Create(ctx, rwColon, false);
                TReservedWord.Create(ctx, rwOpenParenthesis, false);
                
                ParseFields(ctx, parentSymbols, typeDefToFill, rwCloseParenthesis, currentTagSymbol, branchLabels);
                
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

constructor TRecordSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef; isPartial: boolean);
var
    recTypeDef: TRecordTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'RecordSpec';
    start := ctx.Cursor;
    state := tsCorrect;

    if (typeDefToFill <> nil) and (typeDefToFill.kind = tkRecord) and TRecordTypeDef(typeDefToFill).isPartial then
    begin
        recTypeDef := TRecordTypeDef(typeDefToFill);
    end
    else
    begin
        typeDefToFill := TRecordTypeDef.Create(ctx);
        recTypeDef := TRecordTypeDef(typeDefToFill);
        recTypeDef.isPartial := isPartial;
        recTypeDef.definingUnit := ctx.parseUnit;
    end;

    TReservedWord.Create(ctx, rwRecord, true);

    ParseFields(ctx, parentSymbols, typeDefToFill, rwEnd);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
