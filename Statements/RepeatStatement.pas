unit RepeatStatement;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token;

type
    TRepeatStatement = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, Anchors, Symbols, TypedToken, ReservedWord, Identifier, Expression, Statement;

constructor TRepeatStatement.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    expr: TTypedToken;
begin
    ctx.Add(Self);
    tokenName := 'Repeat';
    start := ctx.Cursor;

    if not PeekReservedWord(ctx, rwRepeat) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;
    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwRepeat, true);

    AddAnchor(rwUntil);
    AddAnchor(rwWith);
    AddAnchor(rwFor);
    AddAnchor(rwIf);
    AddAnchor(rwWhile);
    AddAnchor(rwRepeat);
    AddAnchor(rwGoto);
    AddAnchor(rwBegin);
    AddAnchor(pkIdentifier);

    nextTokenKind := SkipUntilAnchor(ctx);
    while (nextTokenKind.reservedWordKind in [rwWith, rwFor, rwIf, rwWhile, rwRepeat, rwGoto, rwBegin])
          or (nextTokenKind.primitiveKind = pkIdentifier)
    do
    begin
        CreateStatement(ctx);
        AddAnchor(rwSemiColon);
        nextTokenKind := SkipUntilAnchor(ctx);
        RemoveAnchor(rwSemiColon);
        TReservedWord.Create(ctx, rwSemiColon, false);

        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwUntil);
    RemoveAnchor(rwWith);
    RemoveAnchor(rwFor);
    RemoveAnchor(rwIf);
    RemoveAnchor(rwWhile);
    RemoveAnchor(rwRepeat);
    RemoveAnchor(rwGoto);
    RemoveAnchor(rwBegin);
    RemoveAnchor(pkIdentifier);

    TReservedWord.Create(ctx, rwUntil, false);
    expr := CreateExpression(ctx);
    if expr.typeDef.kind <> tkBoolean then
    begin
        state := tsError;
        errorMessage := 'Until condition should be boolean, but it is ' + TypeKindStr[ord(expr.typeDef.kind)] + '.';
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
