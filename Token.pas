unit Token;

{$mode objfpc}
{$longstrings on}

interface

type
    TTokenState = (tsCorrect, tsError, tsMissing, tsSkipped, tsEndOf, tsInvisible);
    TToken = class
    protected
        asString: string;
    public
        state: TTokenState;
        start: PChar;
        len: integer;
        tokenName: shortstring;
        line: integer;
        position: integer;
        errorMessage: string;
        isPrimitive: boolean;
        endMarker: TToken;
        function GetStr: string;
        destructor Destroy; override;
    end;

implementation

function TToken.GetStr: string;
begin
    if length(asString) = 0 then
        SetString(asString, start, len);
    GetStr := asString;
end;

destructor TToken.Destroy;
begin
    if endMarker <> nil then
        endMarker.Free;
end;

end.
