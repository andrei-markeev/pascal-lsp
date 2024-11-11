unit FunctionImpl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Token, TypedToken, TypeDefs, Identifier;

type
    TFunctionImpl = class(TToken)
    public
        nameIdent: TIdentifier;
        paramDecls: TTypedTokenArray;
        funcType: TTypeDef;
        returnType: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    Anchors, ReservedWord, Scopes, Symbols, TypeSpec, ParameterDecl, Block;

constructor TFunctionImpl.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextReservedWordKind: TReservedWordKind;
    needsReturnType: boolean;
    symbolKind: TSymbolKind;
    paramDecl: TParameterDecl;
    i: integer;
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

    paramDecls := TTypedTokenArray.Create;

    nextReservedWordKind := DetermineReservedWord(ctx);
    if nextReservedWordKind = rwOpenParenthesis then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);

        hasMoreParams := false;
        repeat
            nextTokenKind := DetermineNextTokenKind(ctx);
            if (nextTokenKind.primitiveKind <> pkIdentifier) and not (nextTokenKind.reservedWordKind in [rwConst, rwVar, rwOut]) then
                break;

            paramDecl := TParameterDecl.Create(ctx, nextTokenKind);
            for i := 0 to length(paramDecl.idents) - 1 do
                paramDecls.Add(paramDecl);

            hasMoreParams := PeekReservedWord(ctx, rwSemiColon);
            if hasMoreParams then
                TReservedWord.Create(ctx, rwSemiColon, true);
        until hasMoreParams = false;

        funcType.parameters := paramDecls;

        TReservedWord.Create(ctx, rwCloseParenthesis, false);
    end;

    if needsReturnType then
    begin
        TReservedWord.Create(ctx, rwColon, false);
        returnType := CreateTypeSpec(ctx).typeDef;
    end;

    RegisterSymbol(nameIdent, nil, symbolKind, funcType, ctx.Cursor);

    // TODO: modifiers

    TReservedWord.Create(ctx, rwSemiColon, false);

    // TODO: asm
    TBlock.Create(ctx);

    TReservedWord.Create(ctx, rwSemiColon, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);

end;

end.
