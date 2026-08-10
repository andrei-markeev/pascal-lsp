unit FileSpec;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, TypeDef, TypeDefs, Token;

type
    TFileSpec = class(TToken)
    public
        constructor Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
    end;

implementation

uses
    CompilationMode, ReservedWord, TypeSpec, FileTypeDef;

constructor TFileSpec.Create(ctx: TParserContext; var typeDefToFill: TTypeDef);
var
    elementTypeDef: TTypeDef;
    fileTypeDef: TFileTypeDef;
begin
    ctx.Add(Self);
    tokenName := 'FileSpec';
    start := ctx.Cursor;

    TReservedWord.Create(ctx, rwFile, true);

    if PeekReservedWord(ctx, rwOf) then
    begin
        TReservedWord.Create(ctx, rwOf, true);
        elementTypeDef := unknownType;
        CreateTypeSpec(ctx, elementTypeDef);
        fileTypeDef := TFileTypeDef.Create(ctx, true, elementTypeDef);
        typeDefToFill := fileTypeDef;
        state := tsCorrect;
    end
    else
    begin
        typeDefToFill := fileType;
        if not (mfUntypedFiles in Features[ctx.mode]) then
        begin
            state := tsError;
            errorMessage := 'Untyped file is not supported in Standard Pascal!';
        end
        else
            state := tsCorrect;
    end;

    ctx.MarkEndOfToken(Self);
end;

end.
