unit TypeDefs;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, CompilationMode;

type
    TTypeKind = (tkUnknown, tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum, tkEnumMember, tkReal, tkString, tkPointer, tkArray, tkDynamicArray, tkRecord, tkObject, tkClass, tkSet, tkFile, tkSubroutine, tkUnitName);

    PTypeDef = ^TTypeDef;
    TTypeDef = record
    case kind: TTypeKind of
        tkInteger: (intSize: integer; isSigned: boolean; rangeStart: int64; rangeEnd: int64);
        tkReal: (realSize: integer);
        tkBoolean: (boolSize: integer);
        tkCharRange: (charRangeStart: char; charRangeEnd: char);
        tkEnum, tkEnumMember: (enumSpec: Pointer);
        tkPointer: (isTyped: boolean; toType: PTypeDef);
        tkArray: (startIndex: integer; endIndex: integer; arrayOfType: PTypeDef);
        tkDynamicArray: (dynArrayOfType: PTypeDef);
    end;

const
    NUM_OF_TYPE_KINDS = 16;
    TypeKindStr: array[0..NUM_OF_TYPE_KINDS - 1] of shortstring = (
        '', 'integer', 'boolean', 'char', 'char range', 'enumeration', 'real', 'string', 'pointer', 'array', 'dynamic array', 'record', 'object', 'class', 'set', 'file'
    );

var
    TypesList: TFPHashList;

    byteType: TTypeDef = (kind: tkInteger; intSize: 1; isSigned: false; rangeStart: 0; rangeEnd: 255);
    shortintType: TTypeDef = (kind: tkInteger; intSize: 1; isSigned: true; rangeStart: -128; rangeEnd: 127);
    wordType: TTypeDef = (kind: tkInteger; intSize: 2; isSigned: false; rangeStart: 0; rangeEnd: 65535);
    smallintType: TTypeDef = (kind: tkInteger; intSize: 2; isSigned: true; rangeStart: -32768; rangeEnd: 32767);
    longwordType: TTypeDef = (kind: tkInteger; intSize: 4; isSigned: false; rangeStart: 0; rangeEnd: 4294967295);
    longintType: TTypeDef = (kind: tkInteger; intSize: 4; isSigned: true; rangeStart: -2147483648; rangeEnd: 2147483647);
    qwordType: TTypeDef = (kind: tkInteger; intSize: 8; isSigned: false);
    int64Type: TTypeDef = (kind: tkInteger; intSize: 8; isSigned: true);

    booleanType: TTypeDef = (kind: tkInteger; boolSize: 1);
    boolean16Type: TTypeDef = (kind: tkInteger; boolSize: 2);
    boolean32Type: TTypeDef = (kind: tkInteger; boolSize: 4);
    boolean64Type: TTypeDef = (kind: tkInteger; boolSize: 8);

    charType: TTypeDef = (kind: tkChar);
    realType: TTypeDef = (kind: tkReal; realSize: 4);
    singleType: TTypeDef = (kind: tkReal; realSize: 4);
    doubleType: TTypeDef = (kind: tkReal; realSize: 8);
    extendedType: TTypeDef = (kind: tkReal; realSize: 10);
    compType: TTypeDef = (kind: tkReal; realSize: 8);
    currencyType: TTypeDef = (kind: tkReal; realSize: 8);

    pointerType: TTypeDef = (kind: tkPointer);
    stringType: TTypeDef = (kind: tkString);

procedure InitPredefinedTypes(mode: TCompilationMode);

implementation

procedure InitPredefinedTypes(mode: TCompilationMode);
begin
    if mode >= cmStandardPascal then
    begin
        TypesList.Add('integer', @smallintType);
        TypesList.Add('boolean', @booleanType);
        TypesList.Add('char', @charType);
        TypesList.Add('real', @realType);
    end;

    if mode >= cmTurboPascal then
    begin
        TypesList.Add('byte', @byteType);
        TypesList.Add('shortint', @shortintType);
        TypesList.Add('word', @wordType);
        TypesList.Add('longint', @longintType);

        TypesList.Add('bytebool', @booleanType);
        TypesList.Add('wordbool', @boolean16Type);
        TypesList.Add('longbool', @boolean32Type);

        TypesList.Add('single', @singleType);
        TypesList.Add('double', @doubleType);
        TypesList.Add('extended', @extendedType);
        TypesList.Add('comp', @compType);

        TypesList.Add('pointer', @pointerType);
    end;

    if mode >= cmFreePascal then
    begin
        TypesList.Add('smallint', @smallintType);
        TypesList.Add('longword', @longwordType);
        TypesList.Add('cardinal', @longwordType);        
        TypesList.Add('qword', @qwordType);
        TypesList.Add('int64', @int64Type);

        TypesList.Add('boolean16', @boolean16Type);
        TypesList.Add('boolean32', @boolean32Type);
        TypesList.Add('boolean64', @boolean64Type);
        TypesList.Add('qwordbool', @boolean64Type);

        TypesList.Add('currency', @currencyType);
    end;
end;

initialization
    TypesList := TFPHashList.Create;
finalization
    TypesList.Free;
end.
