unit PointerTypeDef;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeDef;

type
    TPointerTypeDef = class(TTypeDef)
    public
        isTyped: boolean;
        pointerToType: TTypeDef;
        constructor Create(AIsTyped: boolean = false; APointerToType: TTypeDef = nil; ASize: longword = 8);
    end;

implementation

constructor TPointerTypeDef.Create(AIsTyped: boolean; APointerToType: TTypeDef; ASize: longword);
begin
    inherited Create(tkPointer, ASize);
    isTyped := AIsTyped;
    pointerToType := APointerToType;
end;

end.
