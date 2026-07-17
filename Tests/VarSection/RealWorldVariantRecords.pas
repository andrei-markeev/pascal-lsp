program RealWorldVariantRecords;

type
    TFPHashList = record end;
    TFPList = record end;

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
            tkRecord: (recordFields: TFPHashList);
            tkObject: (objectFields: TFPHashList; parentObject: PTypeDef);
            tkClass: (classFields: TFPHashList; parentClass: PTypeDef);
            tkProcedure, tkFunction: (parameters: Pointer; returnType: PTypeDef; overloads: TFPList);
            tkBoolean, tkChar, tkReal, tkUnitName: ();
    end;

begin
end.
