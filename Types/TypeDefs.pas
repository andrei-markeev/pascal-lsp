unit TypeDefs;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, CompilationMode;

type
    TTypeKind = (
        tkUnknown, tkInteger, tkBoolean, tkChar, tkCharRange, tkEnum, tkEnumMember,
        tkReal, tkString, tkPointer, tkArray, tkDynamicArray,
        tkRecord, tkObject, tkClass, tkSet, tkFile, tkProcedure, tkFunction, tkUnitName
    );

    PTypeDef = ^TTypeDef;
    TTypeDef = record
        size: longword;
    case kind: TTypeKind of
        tkInteger: (isSigned: boolean; rangeStart: int64; rangeEnd: int64);
        tkCharRange: (charRangeStart: char; charRangeEnd: char);
        tkEnum, tkEnumMember: (enumSpec: Pointer);
        tkPointer: (isTyped: boolean; pointerToType: PTypeDef);
        tkArray: (typeOfIndex: PTypeDef; typeOfValues: PTypeDef);
        tkDynamicArray: (typeOfDynValues: PTypeDef);
        tkSet: (typeOfSet: PTypeDef);
        tkRecord: (fields: TFPHashList);
        tkProcedure, tkFunction: (parameters: Pointer);
        tkBoolean, tkChar, tkReal, tkUnitName: ();
    end;

const
    NUM_OF_TYPE_KINDS = 20;
    TypeKindStr: array[0..NUM_OF_TYPE_KINDS - 1] of shortstring = (
        '(unknown)', 'integer', 'boolean', 'char', 'char range', 'enumeration', 'enum value',
        'real', 'string', 'pointer', 'array', 'dynamic array',
        'record', 'object', 'class', 'set', 'file', 'procedure', 'function', 'unit name'
    );

var
    TypesList: TFPHashList;

    byteType: TTypeDef = (size: 1; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 255);
    shortintType: TTypeDef = (size: 1; kind: tkInteger; isSigned: true; rangeStart: -128; rangeEnd: 127);
    wordType: TTypeDef = (size: 2; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 65535);
    smallintType: TTypeDef = (size: 2; kind: tkInteger; isSigned: true; rangeStart: -32768; rangeEnd: 32767);
    longwordType: TTypeDef = (size: 4; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 4294967295);
    longintType: TTypeDef = (size: 4; kind: tkInteger; isSigned: true; rangeStart: -2147483648; rangeEnd: 2147483647);
    qwordType: TTypeDef = (size: 8; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 0);
    int64Type: TTypeDef = (size: 8; kind: tkInteger; isSigned: true; rangeStart: 0; rangeEnd: 0);

    booleanType: TTypeDef = (size: 1; kind: tkBoolean);
    boolean16Type: TTypeDef = (size: 2; kind: tkBoolean);
    boolean32Type: TTypeDef = (size: 4; kind: tkBoolean);
    boolean64Type: TTypeDef = (size: 8; kind: tkBoolean);

    charType: TTypeDef = (size: 1; kind: tkChar);
    realType: TTypeDef = (size: 4; kind: tkReal);
    singleType: TTypeDef = (size: 4; kind: tkReal);
    doubleType: TTypeDef = (size: 4; kind: tkReal);
    extendedType: TTypeDef = (size: 4; kind: tkReal);
    compType: TTypeDef = (size: 8; kind: tkReal);
    currencyType: TTypeDef = (size: 8; kind: tkReal);

    pointer32Type: TTypeDef = (size: 4; kind: tkPointer; isTyped: false; pointerToType: nil);
    pointer64Type: TTypeDef = (size: 8; kind: tkPointer; isTyped: false; pointerToType: nil);

    shortstringType: TTypeDef = (size: 255; kind: tkString);

    ansiString32Type: TTypeDef = (size: 4; kind: tkString);
    ansiString64Type: TTypeDef = (size: 8; kind: tkString);

procedure InitPredefinedTypes(mode: TCompilationMode);
function TypesAreAssignable(left, right: TTypeDef; out errorMessage: string): boolean;

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

        TypesList.Add('pointer', @pointer64Type);
        TypesList.Add('string', @shortstringType);
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

        TypesList.Add('ansistring', @ansiString64Type);
    end;
end;

function TypesAreAssignable(left, right: TTypeDef; out errorMessage: string): boolean;
begin
    TypesAreAssignable := left.kind = right.kind;
    if (left.kind = tkUnknown) or (right.kind = tkUnknown) then
        TypesAreAssignable := true
    else if (left.kind = tkChar) and (right.kind = tkCharRange) then
        TypesAreAssignable := true
    else if (left.kind = tkCharRange) and (right.kind = tkChar) then
        TypesAreAssignable := true
    else if (left.kind in [tkEnum, tkEnumMember]) and (right.kind in [tkEnum, tkEnumMember]) then
        TypesAreAssignable := left.enumSpec = right.enumSpec
    else if (left.kind = tkSet) and (right.kind = tkSet) then
        TypesAreAssignable := TypesAreAssignable(left.typeOfSet^, right.typeOfSet^, errorMessage);

    // TODO: functions

    if not TypesAreAssignable then
        if (left.kind = tkSet) and (right.kind = tkSet) then
            errorMessage := 'base types of sets are not compatible: ' + errorMessage
        else if (left.kind in [tkEnum, tkEnumMember]) and (right.kind in [tkEnum, tkEnumMember]) then
            errorMessage := 'different enums!'
        else
            errorMessage := 'expected ' + TypeKindStr[ord(left.kind)] + ' or assignment-compatible, but found ' + TypeKindStr[ord(right.kind)] + '!';
end;

initialization
    TypesList := TFPHashList.Create;
finalization
    TypesList.Free;
end.
