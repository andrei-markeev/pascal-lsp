unit IntegerTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TIntegerTypeDef = class(TTypeDef)
    public
        isSigned: boolean;
        rangeStart: int64;
        rangeEnd: int64;
        constructor Create(ASize: longword; AIsSigned: boolean; ARangeStart: int64 = 0; ARangeEnd: int64 = 0);
    end;

implementation

constructor TIntegerTypeDef.Create(ASize: longword; AIsSigned: boolean; ARangeStart: int64; ARangeEnd: int64);
begin
    inherited Create(tkInteger, ASize);
    isSigned := AIsSigned;
    rangeStart := ARangeStart;
    rangeEnd := ARangeEnd;
end;

end.
