unit CommonFuncs;

interface

uses
    ParserContext, Token, TypedToken;

type
    TCreateTokenFunc = function(ctx: TParserContext): TToken;
    TCreateTypedTokenFunc = function(ctx: TParserContext): TTypedToken;
    TFindSymbolFunc = function(const name: shortstring): pointer of object;

var
    CommonFunctions: record
        createExpression: TCreateTypedTokenFunc;
        createBlock: TCreateTokenFunc;
        createStatement: TCreateTokenFunc;
        findSymbol: TFindSymbolFunc;
    end;

implementation

end.
