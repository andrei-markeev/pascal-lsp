unit FunctionImpl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypeDefs, Identifier, ParameterDecl;

type
    TFunctionImpl = class(TToken)
    public
        nameIdent: TIdentifier;
        paramDecls: array of TParameterDecl;
        funcType: TTypeDef;
        returnType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, ReservedWord, Scopes, Symbols, TypeSpec, Block;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextReservedWordKind: TReservedWordKind;
    needsReturnType: boolean;
    symbolKind: TSymbolKind;
    l: integer;
    hasMoreParams: boolean;
begin
    ctx.Add(Self);
    tokenName := 'Function';

    ctx.SkipTrivia;
    start := ctx.Cursor;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if not (nextReservedWordKind in [rwFunction, rwProcedure]) then
    begin
        state := tsMissing;
        len := 0;
        exit;
    end;

    needsReturnType := nextReservedWordKind = rwFunction;
    if nextReservedWordKind = rwFunction then
    begin
        symbolKind := skFunction;
        funcType.kind := tkFunction;
        TReservedWord.Create(ctx, rwFunction, true)
    end
    else
    begin
        tokenName := 'Procedure';
        symbolKind := skProcedure;
        funcType.kind := tkProcedure;
        TReservedWord.Create(ctx, rwProcedure, true);
    end;

    nameIdent := TIdentifier.Create(ctx, false);
    RegisterSymbol(nameIdent, nil, symbolKind, funcType, ctx.Cursor);

    nextReservedWordKind := DetermineReservedWord(ctx);
    if nextReservedWordKind = rwOpenParenthesis then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);

        l := 0;
        hasMoreParams := false;
        repeat
            nextTokenKind := DetermineNextTokenKind(ctx);
            if nextTokenKind.primitiveKind <> pkIdentifier then
                break;

            SetLength(paramDecls, l + 1);
            paramDecls[l] := TParameterDecl.Create(ctx);
            inc(l);

            hasMoreParams := PeekReservedWord(ctx, rwSemiColon);
            if hasMoreParams then
                TReservedWord.Create(ctx, rwSemiColon, true);
        until hasMoreParams = false;

        TReservedWord.Create(ctx, rwCloseParenthesis, false);
    end;

    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        returnType := CreateTypeSpec(ctx).typeDef;
    end;

    // TODO: modifiers

    TReservedWord.Create(ctx, rwSemiColon, false);

    // TODO: asm
    TBlock.Create(ctx);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
