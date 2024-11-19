unit Scopes;

{$mode objfpc}
{$longstrings on}

interface

uses contnrs, Token;

type
    TScope = class
    public
        token: TToken;
        parentScope: TScope;
        symbolsList: TFPHashList;
        constructor Create;
        destructor Destroy; override;
    end;

procedure RegisterScope(scopeToken: TToken);
function FindScope(cursor: PChar): TScope;
procedure ResetScopes;

implementation

var ScopesList: array of TScope;

procedure RegisterScope(scopeToken: TToken);
var
    l: integer;
    p: TScope;
begin
    p := FindScope(scopeToken.start);
    l := length(ScopesList);
    SetLength(ScopesList, l + 1);
    ScopesList[l] := TScope.Create;
    ScopesList[l].token := scopeToken;
    ScopesList[l].parentScope := p;
end;

function FindScope(cursor: PChar): TScope;
var
    i: integer;
    unbound: boolean;
    s: TScope;
begin

    Result := ScopesList[0];

    for i := length(ScopesList) - 1 downto 1 do
    begin
        s := ScopesList[i];
        unbound := not s.token.isPrimitive and (s.token.endMarker = nil);
        if (s.token.start <= cursor) and (unbound or (cursor < s.token.start + s.token.len)) then
        begin
            Result := s;
            break;
        end;
    end;

end;

constructor TScope.Create;
begin
    symbolsList := TFPHashList.Create;
end;

destructor TScope.Destroy;
begin
    symbolsList.Free;
end;

procedure ResetScopes;
var
    i: integer;
begin
    for i := 0 to length(ScopesList) - 1 do
        ScopesList[i].Free;
    SetLength(ScopesList, 1);
    ScopesList[0] := TScope.Create;
end;

var i: integer;

initialization
    SetLength(ScopesList, 1);
    ScopesList[0] := TScope.Create;
finalization
    for i := 0 to length(ScopesList) - 1 do
        ScopesList[i].Free;
    SetLength(ScopesList, 0);
end.

end.
