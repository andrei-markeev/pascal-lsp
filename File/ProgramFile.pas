unit ProgramFile;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDefs, Token, ReservedWord, Identifier,
    UsesClause, ConstSection, TypeSection, VarSection, FunctionImpl,
    Block;

type
    TProgramFile = class(TToken)
    public
        constructor Create(ctx: TParserContext);
    end;


implementation

constructor TProgramFile.Create(ctx: TParserContext);
var
    nextIsComma: boolean;
    ident: TIdentifier;
    programTypeDef: TTypeDef;
begin
    tokenName := 'ProgramFile';
    ctx.parseUnit := Self;
    ctx.Add(Self);

    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwProgram, false);

    ident := TIdentifier.Create(ctx, false);
    programTypeDef.kind := tkUnitName;
    RegisterSymbol(ident, nil, skUnitName, programTypeDef, ctx.Cursor);

    // programs parameters are ignored by FPC so we also ignore them
    if PeekReservedWord(ctx, rwOpenParenthesis) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);
        repeat
            TIdentifier.Create(ctx, false);

            if PeekReservedWord(ctx, rwCloseParenthesis) then
                break;

            nextIsComma := PeekReservedWord(ctx, rwComma);
            if nextIsComma then
                TReservedWord.Create(ctx, rwComma, true);
        until not nextIsComma;
        TReservedWord.Create(ctx, rwCloseParenthesis, not nextIsComma);
    end;

    TReservedWord.Create(ctx, rwSemiColon, false);

    if PeekReservedWord(ctx, rwUses) then
        TUsesClause.Create(ctx);

    TBlock.Create(ctx, [], unknownType, unknownType);

    TReservedWord.Create(ctx, rwDot, false);

    ctx.MarkEndOfToken(Self);
end;

end.
