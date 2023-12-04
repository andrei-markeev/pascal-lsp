unit ConstDecl;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Symbols, Token, ReservedWord, Identifier,
    ConstValue in 'Declarations/ConstValue.pas',
    TypeSpec in 'Types/TypeSpec.pas';

type
    TConstDecl = class(TToken)
    public
        ident: TIdentifier;
        constType: TTypeSpec;
        value: TConstValue;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;

implementation

constructor TConstDecl.Create(ctx: TParserContext);
var
    nextReservedWord: TReservedWordKind;
    nextTokenKind: TTokenKind;
    symbolKind: TSymbolKind;
begin
    tokenName := 'ConstDecl';
    ctx.Add(Self);

    ctx.SkipTrivia;
    start := ctx.Cursor;

    nextReservedWord := DetermineReservedWord(ctx);
    if nextReservedWord <> rwUnknown then
    begin
        len := 0;
        state := tsMissing;
        exit;
    end;
    ident := TIdentifier.Create(ctx);
    symbolKind := skConstant;

    AddAnchor(rwEquals);
    AddAnchor(rwColon);
    AddAnchor(pkNumber);
    AddAnchor(pkString);
    AddAnchor(pkIdentifier);
    nextTokenKind := SkipUntilAnchor(ctx);

    if nextTokenKind.reservedWordKind = rwColon then
    begin
        TReservedWord.Create(ctx, rwColon, true);
        constType := TTypeSpec.Create(ctx);
        symbolKind := skTypedConstant;
    end;
    RemoveAnchor(rwColon);

    TReservedWord.Create(ctx, rwEquals, false);
    RemoveAnchor(rwEquals);

    nextTokenKind := SkipUntilAnchor(ctx);
    RemoveAnchor(pkNumber);
    RemoveAnchor(pkString);
    RemoveAnchor(pkIdentifier);

    value := TConstValue.Create(ctx, nextTokenKind);

    len := ctx.Cursor - start;
    RegisterSymbol(ident, symbolKind, ctx.parseUnit, ctx.Cursor);
end;

destructor TConstDecl.Destroy;
begin
    ident.Free;
    value.Free;
end;

end.
