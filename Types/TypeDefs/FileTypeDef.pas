unit FileTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TFileTypeDef = class(TTypeDef)
    public
        isTyped: boolean;
        fileOfTypeDef: TTypeDef;
        constructor Create(ctx: TTypeDefTracker = nil; AIsTyped: boolean = false; AFileOfTypeDef: TTypeDef = nil; ASize: longword = 0);
    end;

implementation

constructor TFileTypeDef.Create(ctx: TTypeDefTracker; AIsTyped: boolean; AFileOfTypeDef: TTypeDef; ASize: longword);
begin
    inherited Create(ctx, tkFile, ASize);
    isTyped := AIsTyped;
    fileOfTypeDef := AFileOfTypeDef;
end;

end.
