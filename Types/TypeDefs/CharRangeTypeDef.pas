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
        constructor Create(ctx: TTypeDefTracker = nil; AStart: char = #0; AEnd: char = #255);
    end;

implementation

constructor TCharRangeTypeDef.Create(ctx: TTypeDefTracker; AStart, AEnd: char);
begin
    inherited Create(ctx, tkCharRange, 1);
    charRangeStart := AStart;
    charRangeEnd := AEnd;
end;

end.
