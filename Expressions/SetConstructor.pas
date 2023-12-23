unit SetConstructor;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypedToken;

type
    TSetConstructor = class(TTypedToken)
    public
        constructor Create(ctx: TParserContext);
    end;

implementation

uses
    TypeDefs, Token, ReservedWord, EnumSpec, Expression;

const
    unknownBaseTypeOfSet: TTypeDef = (size: 1; kind: tkUnknown);

constructor TSetConstructor.Create(ctx: TParserContext);
var
    expr: TTypedToken;
    exprStr: string;
    nextReservedWord: TReservedWordKind;
    baseType: PTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'SetConstructor';
    start := ctx.Cursor;
    state := tsCorrect;

    TReservedWord.Create(ctx, rwOpenSquareBracket, true);

    nextReservedWord := DetermineReservedWord(ctx);

    if nextReservedWord = rwCloseSquareBracket then
    begin
        typeDef.size := 1;
        typeDef.kind := tkSet;
        typeDef.typeOfSet := @unknownBaseTypeOfSet;

        TReservedWord.Create(ctx, rwCloseSquareBracket, false);
        ctx.MarkEndOfToken(Self);
        exit;
    end;

    expr := CreateExpression(ctx);

    if expr.typeDef.kind = tkEnumMember then
        baseType := @TEnumSpec(expr.typeDef.enumSpec).typeDef
    else if expr.typeDef.kind = tkCharRange then
        baseType := @charType
    else
        baseType := @expr.typeDef;

    if not (baseType^.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum]) then
    begin
        state := tsError;
        typeDef.kind := tkSet;
        typeDef.typeOfSet := @unknownBaseTypeOfSet;
        errorMessage := 'Expected a set of ordinal type. Type of set cannot be ' + TypeKindStr[ord(baseType^.kind)];
    end
    else
    begin
        typeDef.size := 1;
        typeDef.kind := tkSet;
        typeDef.typeOfSet := baseType;
    end;

    nextReservedWord := DetermineReservedWord(ctx);
    while nextReservedWord in [rwComma, rwRange] do
    begin
        TReservedWord.Create(ctx, nextReservedWord, true);
        expr := CreateExpression(ctx);
        if (state = tsCorrect) and not TypesAreAssignable(baseType^, expr.typeDef, errorMessage) then
        begin
            typeDef.typeOfSet := @unknownBaseTypeOfSet;
            state := tsError;
            SetString(exprStr, expr.start, expr.len);
            errorMessage := exprStr + ' is not assignable to the type of the set (' + TypeKindStr[ord(baseType^.kind)] + '): ' + errorMessage;
        end;
        // TODO: check that range has lower element first (if expressions are constants)
        nextReservedWord := DetermineReservedWord(ctx);
    end;

    TReservedWord.Create(ctx, rwCloseSquareBracket, false);

    ctx.MarkEndOfToken(Self);
end;

end.
