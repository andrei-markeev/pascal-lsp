unit FunctionImpl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypeDefs, Identifier, VarDecl;

type
    TFunctionImpl = class(TToken)
    public
        nameIdent: TIdentifier;
        paramDecls: array of TVarDecl;
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
    ident: TIdentifier;
    symbolKind: TSymbolKind;
    l: integer;
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
        repeat
            nextTokenKind := DetermineNextTokenKind(ctx);
            if nextTokenKind.primitiveKind = pkIdentifier then
            begin
                // TODO: constant parameters
                // TODO: variable parameters
                // TODO: untyped parameters
                // TODO: open parameters (e.g. open arrays)

                SetLength(paramDecls, l + 1);
                paramDecls[l] := TVarDecl.Create(ctx, [nil]);
                inc(l);

                TReservedWord.Create(ctx, rwSemiColon, false);
            end;
        until nextTokenKind.primitiveKind <> pkIdentifier;

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
