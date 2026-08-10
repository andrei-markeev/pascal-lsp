unit TypecastRef;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef, VarRef;

function IsValidTypecastSize(ctx: TParserContext; targetType, sourceType: TTypeDef; isLHS: boolean): boolean;
function ParseTypecast(ctx: TParserContext; ref: TVarRef; isMaybeLeftHandSide: boolean): boolean;

implementation

uses
    sysutils, CompilationMode, Token, Symbols, TypedToken, ReservedWord, Expression, Designator, TypeDefs;

function IsValidTypecastSize(ctx: TParserContext; targetType, sourceType: TTypeDef; isLHS: boolean): boolean;
var
    targetIsOrdinal, sourceIsOrdinal: boolean;
    targetIsPointer, sourceIsPointer: boolean;
    targetIsClass, sourceIsClass: boolean;
begin
    if (targetType = nil) or (sourceType = nil) then
        exit(true);

    if (targetType.size = 0) or (sourceType.size = 0) or (targetType.size = sourceType.size) then
        exit(true);

    if isLHS then
        exit(false);

    targetIsClass := targetType.kind in [tkClass, tkObject];
    sourceIsClass := sourceType.kind in [tkClass, tkObject];
    targetIsPointer := targetType.kind = tkPointer;
    sourceIsPointer := sourceType.kind = tkPointer;

    if (targetIsClass and sourceIsClass) or
       (targetIsPointer and sourceIsClass) or
       (targetIsClass and sourceIsPointer) then
        exit(true);

    targetIsOrdinal := targetType.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum];
    sourceIsOrdinal := sourceType.kind in [tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum];

    if mfExtendedTypecasting in Features[ctx.mode] then
    begin
        if (targetIsOrdinal or targetIsPointer) and (sourceIsOrdinal or sourceIsPointer) then
            exit(true);
    end
    else if mfBasicTypecasting in Features[ctx.mode] then
    begin
        if targetIsOrdinal and sourceIsOrdinal then
            exit(true);

        if targetIsPointer and sourceIsOrdinal and (sourceType.size = 4) then
            exit(true);
    end;

    Result := false;
end;

function ParseTypecast(ctx: TParserContext; ref: TVarRef; isMaybeLeftHandSide: boolean): boolean;
var
    innerToken: TTypedToken;
begin
    if ref.isSimple and ref.canBeTypecast and (
        ((ref.symbol <> nil) and (ref.symbol.kind = skTypeName)) or
        ((ref.symbol = nil) and (TypesList.Find(LowerCase(ref.firstIdent.name)) <> nil))
    ) then
    begin
        TReservedWord.Create(ctx, rwOpenParenthesis, true);
        if isMaybeLeftHandSide then
            innerToken := CreateDesignator(ctx, true)
        else
            innerToken := CreateExpression(ctx);

        if (innerToken <> nil) and (innerToken.state <> tsError) and (ref.typeDef <> nil) and (innerToken.typeDef <> nil) and not IsValidTypecastSize(ctx, ref.typeDef, innerToken.typeDef, isMaybeLeftHandSide) then
        begin
            ref.state := tsError;
            ref.errorMessage := 'Invalid typecast: type ' + ref.firstIdent.name + '(' + TypeKindStr[ord(ref.typeDef.kind)] + ') has size ' + IntToStr(ref.typeDef.size) + ' but the typecasted variable reference has size ' + IntToStr(innerToken.typeDef.size);
        end;

        TReservedWord.Create(ctx, rwCloseParenthesis, true);
        ref.isSimple := false;
        Result := true;
    end
    else
        Result := false;
end;

end.
