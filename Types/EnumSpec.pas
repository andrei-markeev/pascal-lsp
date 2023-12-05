unit EnumSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, ParserContext, Symbols, TypeDefs, Token, ReservedWord, Identifier;

type
    TEnumSpec = class(TToken)
    public
        typeDef: TTypeDef;
        memberTypeDef: TTypeDef;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TEnumSpec.Create(ctx: TParserContext);
var
    ident: TIdentifier;
    symbol: TSymbol;
    hasMoreMembers: boolean;
begin
    ctx.Add(Self);
    tokenName := 'EnumSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwOpenParenthesis, true);

    typeDef.kind := tkEnum;
    typeDef.values := TFPHashList.Create;
    memberTypeDef.kind := tkEnumMember;
    memberTypeDef.enumSpec := Self;
    repeat
        ident := TIdentifier.Create(ctx);
        symbol := RegisterSymbol(ident, skConstant, ctx.parseUnit, memberTypeDef, ctx.Cursor);
        typeDef.values.Add(ident.GetName, symbol);
        ctx.SkipTrivia;
        // TODO: handle assignments
        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
           TReservedWord.Create(ctx, rwComma, true);
    until hasMoreMembers = false;

    TReservedWord.Create(ctx, rwCloseParenthesis, false);

    state := tsCorrect;
    ctx.MarkEndOfToken(Self);
end;

destructor TEnumSpec.Destroy;
begin
    if typeDef.values <> nil then
        typeDef.values.Free;
end;

end.
