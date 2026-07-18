unit CharRangeTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TCharRangeTypeDef = class(TTypeDef)
    public
        charRangeStart: char;
        charRangeEnd: char;
        constructor Create(AStart, AEnd: char);
    end;

implementation

constructor TCharRangeTypeDef.Create(AStart, AEnd: char);
begin
    inherited Create(tkCharRange, 1);
    charRangeStart := AStart;
    charRangeEnd := AEnd;
end;

end.
