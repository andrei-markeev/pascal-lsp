unit VarDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, TypeDef, TypeDefs, Token, ReservedWord, Identifier, TypeSpec, ConstValue;

type
    TVarDecl = class(TToken)
    private
        procedure ParseDefaultValue(ctx: TParserContext);
    public
        idents: array of TIdentifier;
        varType: TTypeDef;
        defaultValue: TConstValue;
        constructor Create(ctx: TParserContext; parentSymbols: array of TSymbol);
    end;

implementation

uses
    CompilationMode;

procedure TVarDecl.ParseDefaultValue(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    typeError: string;
    defaultValueIsValid: boolean;
begin
    if not PeekReservedWord(ctx, rwEquals) then
        exit;

    TReservedWord.Create(ctx, rwEquals, true);
    nextTokenKind := DetermineNextTokenKind(ctx);
    defaultValue := TConstValue.Create(ctx, nextTokenKind);

    if not (ctx.mode in [cmDelphi, cmFreePascal, cmObjectFreePascal]) then
    begin
        state := tsError;
        errorMessage := 'Default variable values are not supported in this compilation mode!';
        exit;
    end;

    defaultValueIsValid := (defaultValue <> nil) and (defaultValue.state <> tsError) and (defaultValue.typeDef <> nil);
    if (varType <> nil) and defaultValueIsValid and not TypesAreAssignable(ctx, varType, defaultValue.typeDef, typeError) then
    begin
        state := tsError;
        errorMessage := 'Variable default value cannot be assigned to the specified type: ' + typeError;
    end;
end;

constructor TVarDecl.Create(ctx: TParserContext; parentSymbols: array of TSymbol);
var
    nextTokenKind: TTokenKind;
    i, l, p: integer;
    hasMoreMembers: boolean;
    symbols: array of TSymbol;
begin
    tokenName := 'VarDecl';
    ctx.Add(Self);
    defaultValue := nil;

    start := ctx.Cursor;

    AddAnchor(pkIdentifier);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(pkIdentifier);

    if nextTokenKind.primitiveKind <> pkIdentifier then
    begin
        SetLength(idents, 0);
        len := 0;
        state := tsMissing;
        exit;
    end;
    start := ctx.Cursor;
    l := 0;
    repeat
        SetLength(idents, l + 1);
        idents[l] := TIdentifier.Create(ctx, false);
        inc(l);
        ctx.SkipTrivia;
        hasMoreMembers := PeekReservedWord(ctx, rwComma);
        if hasMoreMembers then
           TReservedWord.Create(ctx, rwComma, true);
    until hasMoreMembers = false;

    AddAnchor(rwColon);
    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(rwColon);

    varType := unknownType;

    SetLength(symbols, l * length(parentSymbols));
    for p := 0 to length(parentSymbols) - 1 do
        for i := 0 to l - 1 do
            symbols[i + p * l] := RegisterSymbol(idents[i], parentSymbols[p], skVariable, varType, ctx.Cursor);

    TReservedWord.Create(ctx, rwColon, nextTokenKind.reservedWordKind = rwColon);
    TTypeSpec.Create(ctx, symbols, varType);

    // Update registered symbols if varType reference changed during TTypeSpec.Create
    for p := 0 to length(parentSymbols) - 1 do
        for i := 0 to l - 1 do
            symbols[i + p * l].typeDef := varType;

    ParseDefaultValue(ctx);

    ctx.MarkEndOfToken(Self);
end;

end.
