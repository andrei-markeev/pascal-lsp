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

uses
    EnumTypeDef, EnumMemberTypeDef;

constructor TEnumSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    hasMoreMembers: boolean;
    ident: TIdentifier;
    enumTypeDef: TEnumTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'EnumSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwOpenParenthesis, true);

    enumTypeDef := TEnumTypeDef.Create(ctx, Self);
    typeDefToFill := enumTypeDef;

    memberTypeDef := TEnumMemberTypeDef.Create(ctx, enumTypeDef, Self);
    repeat
        ident := TIdentifier.Create(ctx, false);
        RegisterSymbol(ident, nil, skConstant, memberTypeDef, ctx.Cursor);

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
