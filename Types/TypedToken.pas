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

    TTypedTokenArray = class
    public
        count: integer;
        items: array of TTypedToken;
        constructor Create;
        procedure Add(tokenToAdd: TTypedToken);
        destructor Destroy; override;
    end;

implementation

constructor TTypedTokenArray.Create;
begin
    SetLength(items, 0);
end;

procedure TTypedTokenArray.Add(tokenToAdd: TTypedToken);
var
    l: integer;
begin
    l := length(items);
    count := l + 1;
    SetLength(items, l + 1);
    items[l] := tokenToAdd;
end;

destructor TTypedTokenArray.Destroy;
var
    i: integer;
begin
    for i := 0 to length(items) - 1 do
        items[i].Free;
    SetLength(items, 0);
end;

end.
