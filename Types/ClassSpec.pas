unit ClassSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, Token;

type
    TClassSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    CompilationMode, Anchors, ReservedWord, Identifier, VarDecl, FunctionDecl, TypeDef, ClassTypeDef;

procedure SetVisibility(ctx: TParserContext; const value: TVisibility; out res: TVisibility);
begin
    res := value;
    TIdentifier.Create(ctx, false);
end;

constructor TClassSpec.Create(ctx: TParserContext; parentSymbols: array of TSymbol; var typeDefToFill: TTypeDef);
var
    i: integer;
    fieldDecl: TVarDecl;
    funcDecl: TFunctionDecl;
    nextTokenKind: TTokenKind;
    s: string;
    visibility: TVisibility;
    ident: TIdentifier;
    symbol: TSymbol;
    found: pointer;
    isParenthesisOrComma: boolean;
    isDeclaredClass: boolean;
    savedCursor: PChar;
    savedCursorBeforeTrivia: PChar;
    savedTokensLen: integer;
    posBeforeTrivia: PChar;
    identToken: TToken;
    currParent: TTypeDef;
    parentTypeDef: TTypeDef;
    depth: integer;
    isCircular: boolean;
    classTypeDef: TClassTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'ClassSpec';
    start := ctx.Cursor;
    state := tsCorrect;

    classTypeDef := TClassTypeDef.Create(ctx);
    typeDefToFill := classTypeDef;
    for i := 0 to high(parentSymbols) do
        if parentSymbols[i] <> nil then
            parentSymbols[i].typeDef := typeDefToFill;

    classTypeDef.classFields.Add('Free', voidProcedureType);

    // TODO: packed classes

    TReservedWord.Create(ctx, rwClass, true);

    // TODO: abstract, sealed

    classTypeDef.parentClass := nil;
    if PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);
        posBeforeTrivia := ctx.GetCursorBeforeTrivia;
        
        if DetermineNextTokenKind(ctx).primitiveKind = pkIdentifier then
        begin
            savedCursor := ctx.Cursor;
            savedCursorBeforeTrivia := ctx.cursorBeforeTrivia;
            savedTokensLen := ctx.tokensLen;
            
            ident := TIdentifier.Create(ctx, false);
            
            nextTokenKind := DetermineNextTokenKind(ctx);
            isParenthesisOrComma := nextTokenKind.reservedWordKind in [rwCloseParenthesis, rwComma];
            
            isDeclaredClass := false;
            if ident.state <> tsMissing then
            begin
                symbol := FindSymbol(ident.GetStr(), ctx.Cursor);
                if (symbol <> nil) and (symbol.kind = skTypeName) and (symbol.typeDef <> nil) and (symbol.typeDef.kind = tkClass) then
                    isDeclaredClass := true
                else
                begin
                    found := TypesList.Find(LowerCase(ident.GetStr()));
                    if (found <> nil) and (TTypeDef(found).kind = tkClass) then
                        isDeclaredClass := true;
                end;
            end;
            
            ident.Free;
            
            ctx.Cursor := savedCursor;
            ctx.cursorBeforeTrivia := savedCursorBeforeTrivia;
            ctx.tokensLen := savedTokensLen;
            
            if isParenthesisOrComma or isDeclaredClass then
            begin
                ident := TIdentifier.Create(ctx, false);
                if isDeclaredClass then
                begin
                    symbol := FindSymbol(ident.GetStr(), ctx.Cursor);
                    if (symbol <> nil) and (symbol.kind = skTypeName) and (symbol.typeDef <> nil) and (symbol.typeDef.kind = tkClass) then
                        parentTypeDef := symbol.typeDef
                    else
                    begin
                        symbol := nil;
                        found := TypesList.Find(LowerCase(ident.GetStr()));
                        if (found <> nil) and (TTypeDef(found).kind = tkClass) then
                            parentTypeDef := TTypeDef(found)
                        else
                            parentTypeDef := nil;
                    end;

                    if parentTypeDef <> nil then
                    begin
                        isCircular := false;
                        currParent := parentTypeDef;
                        depth := 0;
                        while (currParent <> nil) and (depth < 100) do
                        begin
                            inc(depth);
                            if currParent = typeDefToFill then
                            begin
                                isCircular := true;
                                break;
                            end;
                            if currParent is TClassTypeDef then
                                currParent := TClassTypeDef(currParent).parentClass
                            else
                                currParent := nil;
                        end;

                        if isCircular then
                        begin
                            ident.state := tsError;
                            ident.errorMessage := 'Circular inheritance is not allowed!';
                        end
                        else
                        begin
                            classTypeDef.parentClass := parentTypeDef;
                            if symbol <> nil then
                                symbol.AddReference(ident);
                        end;
                    end;
                end
                else
                begin
                    ident.state := tsError;
                    ident.errorMessage := 'Class type expected!';
                end;
                
                while PeekReservedWord(ctx, rwComma) do
                begin
                    TReservedWord.Create(ctx, rwComma, true);
                    if DetermineNextTokenKind(ctx).primitiveKind = pkIdentifier then
                        TIdentifier.Create(ctx, false);
                end;
                
                TReservedWord.Create(ctx, rwCloseParenthesis, false);
            end
            else
            begin
                // The parenthesis was empty or has no valid class heritage.
                // Temporarily rewind Cursor to '(' (posBeforeTrivia - 1) to place both missing tokens before the newline.
                savedCursor := ctx.Cursor;
                ctx.Cursor := posBeforeTrivia - 1;
                
                identToken := TIdentifier.Create(ctx, false);
                identToken.start := posBeforeTrivia;
                
                identToken := TReservedWord.Create(ctx, rwCloseParenthesis, false);
                identToken.start := posBeforeTrivia;
                
                ctx.Cursor := savedCursor;
            end;
        end
        else
        begin
            savedCursor := ctx.Cursor;
            ctx.Cursor := posBeforeTrivia - 1;
            identToken := TIdentifier.Create(ctx, false);
            identToken.start := posBeforeTrivia;
            ctx.Cursor := savedCursor;
            
            TReservedWord.Create(ctx, rwCloseParenthesis, false);
        end;
    end;

    visibility := vPublic;
    nextTokenKind := DetermineNextTokenKind(ctx);

    while (nextTokenKind.primitiveKind = pkIdentifier) or (nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor]) do
    begin
        if nextTokenKind.primitiveKind = pkIdentifier then
        begin
            s := LowerCase(PeekIdentifier(ctx));
            case s of
                'private': SetVisibility(ctx, vPrivate, visibility);
                'public': SetVisibility(ctx, vPublic, visibility);
                'protected': if ctx.mode >= cmObjectFreePascal then SetVisibility(ctx, vProtected, visibility);
            else
                fieldDecl := TVarDecl.Create(ctx, parentSymbols);
                for i := 0 to length(fieldDecl.idents) - 1 do
                begin
                    if fieldDecl.varType <> nil then
                    begin
                        if fieldDecl.varType.visibility <> visibility then
                        begin
                            fieldDecl.varType := fieldDecl.varType.Clone;
                            fieldDecl.varType.visibility := visibility;
                        end;
                        inc(classTypeDef.size, fieldDecl.varType.size);
                    end;
                    classTypeDef.classFields.Add(fieldDecl.idents[i].GetStr(), fieldDecl.varType);
                end;
                TReservedWord.Create(ctx, rwSemiColon, false);
            end;

            // TODO: static modifier
        end
        else if nextTokenKind.reservedWordKind in [rwProcedure, rwFunction, rwConstructor, rwDestructor] then
        begin
            funcDecl := TFunctionDecl.Create(ctx, nextTokenKind.reservedWordKind, parentSymbols);
            if funcDecl.funcType <> nil then
                funcDecl.funcType.visibility := visibility;
            classTypeDef.classFields.Add(funcDecl.nameIdent.GetStr(), funcDecl.funcType);
        end;

        nextTokenKind := DetermineNextTokenKind(ctx);
    end;

    TReservedWord.Create(ctx, rwEnd, true);

    ctx.MarkEndOfToken(Self);
end;

end.
