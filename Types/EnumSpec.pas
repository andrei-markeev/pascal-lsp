unit EnumSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Symbols, TypeDefs, Token, ReservedWord, Identifier;

type
    TEnumSpec = class(TToken)
    public
        memberTypeDef: TTypeDef;
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
    end;

implementation

constructor TEnumSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    hasMoreMembers: boolean;
    ident: TIdentifier;
begin
    ctx.Add(Self);
    tokenName := 'EnumSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwOpenParenthesis, true);

    typeDefToFill.kind := tkEnum;
    typeDefToFill.enumType := @typeDefToFill;
    typeDefToFill.enumSpec := Self;
    memberTypeDef.kind := tkEnumMember;
    memberTypeDef.enumType := @typeDefToFill;
    memberTypeDef.enumSpec := Self;
    repeat
        ident := TIdentifier.Create(ctx, false);
        RegisterSymbol(ident, nil, skConstant, @memberTypeDef, ctx.Cursor);

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
