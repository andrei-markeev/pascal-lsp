unit EnumSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, TypeDefs, Token, ReservedWord, Identifier;

type
    TEnumSpec = class(TToken)
    public
        typeDef: TTypeDef;
        constructor Create(ctx: TParserContext);
    end;

implementation

constructor TEnumSpec.Create(ctx: TParserContext);
var
    memberTypeDef: TTypeDef;
    hasMoreMembers: boolean;
    ident: TIdentifier;
begin
    ctx.Add(Self);
    tokenName := 'EnumSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwOpenParenthesis, true);

    typeDef.kind := tkEnum;
    typeDef.enumSpec := Self;
    memberTypeDef.kind := tkEnumMember;
    memberTypeDef.enumSpec := Self;
    repeat
        ident := TIdentifier.Create(ctx, false);
        RegisterSymbol(ident, skConstant, ctx.parseUnit, memberTypeDef, ctx.Cursor);

        ctx.SkipTrivia;
        // TODO: support number assignments
        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
            TReservedWord.Create(ctx, rwComma, true);
    until hasMoreMembers = false;

    TReservedWord.Create(ctx, rwCloseParenthesis, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

end.
