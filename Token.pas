unit Token;

{$mode objfpc}
{$longstrings on}

interface

type
    TTokenState = (tsCorrect, tsError, tsMissing, tsSkipped, tsEndOf);
    TToken = class
    public
        state: TTokenState;
        start: PChar;
        len: integer;
        tokenName: shortstring;
        line: integer;
        position: integer;
        errorMessage: string;
        isPrimitive: boolean;
    end;

implementation

end.
