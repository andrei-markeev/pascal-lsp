unit CommonFuncs;

interface

uses
    Anchors, ParserContext, Token, TypedToken;

type
    TCreateTokenFunc = function(ctx: TParserContext): TToken;
    TCreateTypedTokenFunc = function(ctx: TParserContext): TTypedToken;

var
    CommonFunctions: record
        createExpression: TCreateTypedTokenFunc;
        createBlock: TCreateTokenFunc;
        createStatement: TCreateTokenFunc;
    end;

implementation

end.