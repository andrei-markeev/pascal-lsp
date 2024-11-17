unit TypedToken;

{$mode objfpc}
{$longstrings on}

interface

uses
    Token, TypeDefs;

type
    TTypedToken = class(TToken)
    public
        typeDef: TTypeDef;
    end;

implementation

end.
