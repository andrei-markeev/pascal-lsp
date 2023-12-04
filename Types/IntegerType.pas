unit IntegerType;

{$mode objfpc}
{$longstrings on}

interface

uses
    TypeRef;

type

    TIntegerType = class(TTypeRef)
    public
        rangeStart: integer;
        rangeEnd: integer;
        size: byte;
        isSigned: boolean;
        constructor Create(typeName: shortstring; typeSize: byte; isSigned: boolean);
        destructor Destroy; override;
    end;

implementation

constructor TIntegerType.Create(typeName: shortstring; typeSize: byte; typeIsSigned: boolean);
begin
    name := typeName;
    size := typeSize;
    isSigned := typeIsSigned;
end;

destructor TIntegerType.Destroy;
begin
    inherited;
end;

end.