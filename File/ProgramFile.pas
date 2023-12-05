unit ProgramFile;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, Anchors, Token, ReservedWord, Identifier,
    UsesClause, ConstSection, VarSection,
    Block;

type
    TProgramFile = class(TToken)
    public
        name: TIdentifier;
        usesClause: TUsesClause;
        implementations: array of TToken;
        initSection: TToken;
        constructor Create(ctx: TParserContext);
        destructor Destroy; override;
    end;


implementation

constructor TProgramFile.Create(ctx: TParserContext);
var
    nextTokenKind: TTokenKind;
    nextIsComma: boolean;
begin
    tokenName := 'ProgramFile';
    ctx.parseUnit := Self;
    ctx.Add(Self);

    start := ctx.Cursor;
    TReservedWord.Create(ctx, rwProgram, false);

    name := TIdentifier.Create(ctx);

    // programs parameters are ignored by FPC so we also ignore them
    ctx.SkipTrivia;
    if ctx.Cursor[0] = '(' then
    begin
        repeat
            TIdentifier.Create(ctx);

            ctx.SkipTrivia;
            if ctx.Cursor[0] = ')' then
            begin
                TReservedWord.Create(ctx, rwCloseParenthesis, true);
                break;
            end;

            nextIsComma := PeekReservedWord(ctx, rwComma);
            if nextIsComma then
                TReservedWord.Create(ctx, rwComma, true);
        until not nextIsComma;
    end;

    TReservedWord.Create(ctx, rwSemiColon, false);

    if PeekReservedWord(ctx, rwUses) then
        usesClause := TUsesClause.Create(ctx);

    AddAnchor(rwConst);
    AddAnchor(rwType);
    AddAnchor(rwVar);
    AddAnchor(rwProcedure);
    AddAnchor(rwFunction);
    AddAnchor(rwBegin);
    AddAnchor(rwEnd);

    nextTokenKind := SkipUntilAnchor(ctx);
    while nextTokenKind.reservedWordKind in [rwConst, rwType, rwVar, rwProcedure, rwFunction] do
    begin
        len := length(implementations);
        SetLength(implementations, len + 1);
        case nextTokenKind.reservedWordKind of
            rwConst: implementations[len] := TConstSection.Create(ctx);
            //rwType: implementations[len] := TTypeSection.Create(ctx);
            rwVar: implementations[len] := TVarSection.Create(ctx);
            //rwProcedure: implementations[len] := TProcedureImpl.Create(ctx);
            //rwFunction: implementations[len] := TFunctionImpl.Create(ctx);
        end;
        nextTokenKind := SkipUntilAnchor(ctx);
    end;

    RemoveAnchor(rwConst);
    RemoveAnchor(rwType);
    RemoveAnchor(rwVar);
    RemoveAnchor(rwProcedure);
    RemoveAnchor(rwFunction);
    RemoveAnchor(rwBegin);
    RemoveAnchor(rwEnd);

    initSection := TBlock.Create(ctx);

    TReservedWord.Create(ctx, rwDot, false);

    ctx.MarkEndOfToken(Self);
end;

destructor TProgramFile.Destroy;
var
    i: integer;
begin
    if usesClause <> nil then
        usesClause.Free;
    for i := 0 to length(implementations) - 1 do
        implementations[i].Free;
    initSection.Free;
end;

end.