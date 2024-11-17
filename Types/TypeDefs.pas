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

    TVisibility = (vPublic, vPrivate, vProtected, vUnknown);

    PTypeDef = ^TTypeDef;
    TTypeDef = record
        size: longword;
        visibility: TVisibility;
    case kind: TTypeKind of
        tkInteger: (isSigned: boolean; rangeStart: int64; rangeEnd: int64);
        tkCharRange: (charRangeStart: char; charRangeEnd: char);
        tkEnum, tkEnumMember: (enumType: PTypeDef; enumSpec: Pointer);
        tkPointer: (isTyped: boolean; pointerToType: PTypeDef);
        tkArray: (typeOfIndex: PTypeDef; typeOfValues: PTypeDef);
        tkDynamicArray: (typeOfDynValues: PTypeDef);
        tkSet: (typeOfSet: PTypeDef);
        tkRecord, tkObject, tkClass: (fields: TFPHashList);
        tkProcedure, tkFunction: (parameters: Pointer; returnType: PTypeDef);
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

    unknownType: TTypeDef = (size: 0; visibility: vPublic; kind: tkUnknown);

    byteType: TTypeDef = (size: 1; visibility: vPublic; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 255);
    shortintType: TTypeDef = (size: 1; visibility: vPublic; kind: tkInteger; isSigned: true; rangeStart: -128; rangeEnd: 127);
    wordType: TTypeDef = (size: 2; visibility: vPublic; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 65535);
    smallintType: TTypeDef = (size: 2; visibility: vPublic; kind: tkInteger; isSigned: true; rangeStart: -32768; rangeEnd: 32767);
    longwordType: TTypeDef = (size: 4; visibility: vPublic; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 4294967295);
    longintType: TTypeDef = (size: 4; visibility: vPublic; kind: tkInteger; isSigned: true; rangeStart: -2147483648; rangeEnd: 2147483647);
    qwordType: TTypeDef = (size: 8; visibility: vPublic; kind: tkInteger; isSigned: false; rangeStart: 0; rangeEnd: 0);
    int64Type: TTypeDef = (size: 8; visibility: vPublic; kind: tkInteger; isSigned: true; rangeStart: 0; rangeEnd: 0);

    booleanType: TTypeDef = (size: 1; visibility: vPublic; kind: tkBoolean);
    boolean16Type: TTypeDef = (size: 2; visibility: vPublic; kind: tkBoolean);
    boolean32Type: TTypeDef = (size: 4; visibility: vPublic; kind: tkBoolean);
    boolean64Type: TTypeDef = (size: 8; visibility: vPublic; kind: tkBoolean);

    charType: TTypeDef = (size: 1; visibility: vPublic; kind: tkChar);
    realType: TTypeDef = (size: 4; visibility: vPublic; kind: tkReal);
    singleType: TTypeDef = (size: 4; visibility: vPublic; kind: tkReal);
    doubleType: TTypeDef = (size: 4; visibility: vPublic; kind: tkReal);
    extendedType: TTypeDef = (size: 4; visibility: vPublic; kind: tkReal);
    compType: TTypeDef = (size: 8; visibility: vPublic; kind: tkReal);
    currencyType: TTypeDef = (size: 8; visibility: vPublic; kind: tkReal);

    pointer32Type: TTypeDef = (size: 4; visibility: vPublic; kind: tkPointer; isTyped: false; pointerToType: nil);
    pointer64Type: TTypeDef = (size: 8; visibility: vPublic; kind: tkPointer; isTyped: false; pointerToType: nil);

    pcharType: TTypeDef = (size: 8; visibility: vPublic; kind: tkPointer; isTyped: true; pointerToType: @charType);

    shortstringType: TTypeDef = (size: 256; visibility: vPublic; kind: tkString);

    ansiString32Type: TTypeDef = (size: 4; visibility: vPublic; kind: tkString);
    ansiString64Type: TTypeDef = (size: 8; visibility: vPublic; kind: tkString);

    voidProcedureType: TTypeDef; // defined in initialization block below

procedure InitPredefinedTypes(mode: TCompilationMode);
function TypesAreAssignable(left, right: TTypeDef; out errorMessage: string): boolean;

implementation

uses
    Parameters;

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
        TypesList.Add('pchar', @pcharType);
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
        TypesList.Add('shortstring', @shortstringType);
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
    voidProcedureType.kind := tkProcedure;
    voidProcedureType.size := 0;
    voidProcedureType.parameters := TParameterList.Create;
finalization
    TypesList.Free;
end.
