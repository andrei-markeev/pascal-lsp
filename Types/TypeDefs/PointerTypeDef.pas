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
        constructor Create(ctx: TObject = nil; AIsTyped: boolean = false; APointerToType: TTypeDef = nil; ASize: longword = 8);
    end;

implementation

constructor TPointerTypeDef.Create(ctx: TObject; AIsTyped: boolean; APointerToType: TTypeDef; ASize: longword);
begin
    inherited Create(ctx, tkPointer, ASize);
    isTyped := AIsTyped;
    pointerToType := APointerToType;
end;

end.
