unit TypeDefs;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs;

type
    TTypeKind = (tkUnknown, tkInteger, tkBoolean, tkChar, tkEnum, tkReal, tkString, tkPointer, tkArray, tkDynamicArray, tkRecord, tkObject, tkClass, tkSet, tkFile);

    PTypeDef = ^TTypeDef;
    TTypeDef = record
    case kind: TTypeKind of
        tkInteger: (intSize: integer; isSigned: boolean; rangeStart: integer; rangeEnd: integer);
        tkBoolean: (boolSize: integer);
        tkEnum: (values: TFPHashList);
        tkPointer: (isTyped: boolean; toType: PTypeDef);
        tkArray: (startIndex: integer; endIndex: integer; arrayOfType: PTypeDef);
        tkDynamicArray: (dynArrayOfType: PTypeDef);
    end;

var
    TypesList: TFPHashList;

implementation

initialization
    TypesList := TFPHashList.Create;
finalization
    TypesList.Free;
end.
