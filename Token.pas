unit Token;

{$mode objfpc}
{$longstrings on}

interface

type
    TTokenState = (tsCorrect, tsError, tsMissing, tsSkipped);
    TToken = class
    public
        state: TTokenState;
        start: PChar;
        len: integer;
        tokenName: shortstring;
        line: integer;
        position: integer;
        errorMessage: string;
    end;

implementation

end.
