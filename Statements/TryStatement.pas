unit TryStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TTryStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, TypeDef, Anchors, TypedToken, ReservedWord, Identifier, TypeDecl, TypeSpec, ParameterDecl, Statement;

constructor TTryStatement.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    hasHandler: boolean;
    dummyTypeDef: TTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'TryStatement';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwTry) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    TReservedWord.Create(ctx, rwTry, false);

    AddAnchor(rwEnd);
    AddAnchor(rwExcept);
    AddAnchor(rwFinally);
    AddAnchor(rwWith);
    AddAnchor(rwFor);
    AddAnchor(rwCase);
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwTry);
    AddAnchor(rwGoto);
    AddAnchor(rwBegin);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwTry, rwGoto, rwBegin])
          or (nextTokenKind.primitiveKind = pkIdentifier)
    do
    begin
        CreateStatement(ctx);
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        if PeekReservedWord(ctx, rwSemiColon) then
            TReservedWord.Create(ctx, rwSemiColon, false);

        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    hasHandler := false;

    if PeekReservedWord(ctx, rwFinally) then
    begin
        TReservedWord.Create(ctx, rwFinally, false);
        hasHandler := true;
        nextTokenKind := SkipUntilAnchor(ctx);
        while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwTry, rwGoto, rwBegin])
              or (nextTokenKind.primitiveKind = pkIdentifier)
        do
        begin
            CreateStatement(ctx);
            AddAnchor(rwSemiColon);
            nextTokenKind := SkipUntilAnchor(ctx);
            RemoveAnchor(rwSemiColon);
            if PeekReservedWord(ctx, rwSemiColon) then
                TReservedWord.Create(ctx, rwSemiColon, false);

            nextTokenKind := SkipUntilAnchor(ctx);
        end;
    end
    else if PeekReservedWord(ctx, rwExcept) then
    begin
        TReservedWord.Create(ctx, rwExcept, false);
        hasHandler := true;
        nextTokenKind := SkipUntilAnchor(ctx);
        while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwCase, rwIf, rwWhile, rwRepeat, rwTry, rwGoto, rwBegin, rwOn])
              or (nextTokenKind.primitiveKind = pkIdentifier)
        do
        begin
            if PeekReservedWord(ctx, rwOn) then
            begin
                TReservedWord.Create(ctx, rwOn, false);
                if DetermineNextTokenKind(ctx).primitiveKind = pkIdentifier then
                begin
                    TIdentifier.Create(ctx, true);
                    if PeekReservedWord(ctx, rwColon) then
                    begin
                        TReservedWord.Create(ctx, rwColon, false);
                        dummyTypeDef := nil;
                        CreateTypeSpec(ctx, dummyTypeDef);
                    end;
                end;
                if PeekReservedWord(ctx, rwDo) then
                    TReservedWord.Create(ctx, rwDo, false);
                CreateStatement(ctx);
            end
            else
                CreateStatement(ctx);

            AddAnchor(rwSemiColon);
            nextTokenKind := SkipUntilAnchor(ctx);
            RemoveAnchor(rwSemiColon);
            if PeekReservedWord(ctx, rwSemiColon) then
                TReservedWord.Create(ctx, rwSemiColon, false);

            nextTokenKind := SkipUntilAnchor(ctx);
        end;
    end;

    RemoveAnchor(rwEnd);
    RemoveAnchor(rwExcept);
    RemoveAnchor(rwFinally);
    RemoveAnchor(rwWith);
    RemoveAnchor(rwFor);
    RemoveAnchor(rwCase);
    RemoveAnchor(rwIf);
    RemoveAnchor(rwWhile);
    RemoveAnchor(rwRepeat);
    RemoveAnchor(rwTry);
    RemoveAnchor(rwGoto);
    RemoveAnchor(rwBegin);
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwEnd, false);

    ctx.MarkEndOfToken(Self);
end;

end.
