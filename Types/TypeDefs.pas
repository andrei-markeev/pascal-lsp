unit TypeDefs;

{$mode objfpc}
{$longstrings on}

interface

uses
    contnrs, classes, CompilationMode,
    TypeDef, IntegerTypeDef, CharRangeTypeDef, EnumTypeDef, EnumMemberTypeDef,
    PointerTypeDef, ArrayTypeDef, DynamicArrayTypeDef, SetTypeDef, RecordTypeDef,
    ObjectTypeDef, ClassTypeDef, RoutineTypeDef, PrimitiveTypeDef;

type
    TTypeKind = TypeDef.TTypeKind;
    TVisibility = TypeDef.TVisibility;
    TTypeDef = TypeDef.TTypeDef;

const
    NUM_OF_TYPE_KINDS = 20;
    TypeKindStr: array[0..NUM_OF_TYPE_KINDS - 1] of shortstring = (
        '(unknown)', 'integer', 'boolean', 'char', 'char range', 'enumeration', 'enum value',
        'real', 'string', 'pointer', 'array', 'dynamic array',
        'record', 'object', 'class', 'set', 'file', 'procedure', 'function', 'unit name'
    );

var
    TypesList: TFPHashList;

    unknownType: TTypeDef;

    byteType: TTypeDef;
    shortintType: TTypeDef;
    wordType: TTypeDef;
    smallintType: TTypeDef;
    longwordType: TTypeDef;
    longintType: TTypeDef;
    qwordType: TTypeDef;
    int64Type: TTypeDef;

    booleanType: TTypeDef;
    boolean16Type: TTypeDef;
    boolean32Type: TTypeDef;
    boolean64Type: TTypeDef;

    charType: TTypeDef;
    realType: TTypeDef;
    singleType: TTypeDef;
    doubleType: TTypeDef;
    extendedType: TTypeDef;
    compType: TTypeDef;
    currencyType: TTypeDef;

    pointer32Type: TTypeDef;
    pointer64Type: TTypeDef;
    pcharType: TTypeDef;

    shortstringType: TTypeDef;
    ansiString32Type: TTypeDef;
    ansiString64Type: TTypeDef;

    voidProcedureType: TTypeDef;

procedure InitPredefinedTypes(mode: TCompilationMode);
function TypesAreAssignable(left, right: TTypeDef; out errorMessage: string): boolean;
function HaveSameSignature(a, b: TTypeDef): boolean;

implementation

uses
    Parameters;

procedure InitPredefinedTypes(mode: TCompilationMode);
begin
    if mode >= cmStandardPascal then
    begin
        TypesList.Add('integer', smallintType);
        TypesList.Add('boolean', booleanType);
        TypesList.Add('char', charType);
        TypesList.Add('real', realType);
    end;

    if mode >= cmExtendedPascal then
    begin
        TypesList.Add('string', shortstringType);
    end;

    if mode >= cmTurboPascal then
    begin
        TypesList.Add('byte', byteType);
        TypesList.Add('shortint', shortintType);
        TypesList.Add('word', wordType);
        TypesList.Add('longint', longintType);

        TypesList.Add('bytebool', booleanType);
        TypesList.Add('wordbool', boolean16Type);
        TypesList.Add('longbool', boolean32Type);

        TypesList.Add('single', singleType);
        TypesList.Add('double', doubleType);
        TypesList.Add('extended', extendedType);
        TypesList.Add('comp', compType);

        TypesList.Add('pointer', pointer64Type);
        TypesList.Add('pchar', pcharType);
    end;

    if mode >= cmFreePascal then
    begin
        TypesList.Add('smallint', smallintType);
        TypesList.Add('longword', longwordType);
        TypesList.Add('cardinal', longwordType);        
        TypesList.Add('qword', qwordType);
        TypesList.Add('int64', int64Type);

        TypesList.Add('boolean16', boolean16Type);
        TypesList.Add('boolean32', boolean32Type);
        TypesList.Add('boolean64', boolean64Type);
        TypesList.Add('qwordbool', boolean64Type);

        TypesList.Add('currency', currencyType);

        TypesList.Add('ansistring', ansiString64Type);
        TypesList.Add('shortstring', shortstringType);
    end;
end;

function TypesAreAssignable(left, right: TTypeDef; out errorMessage: string): boolean;
begin
    if (left = nil) or (right = nil) then
    begin
        TypesAreAssignable := true;
        exit;
    end;

    TypesAreAssignable := left.kind = right.kind;
    if (left.kind = tkUnknown) or (right.kind = tkUnknown) then
        TypesAreAssignable := true
    else if (left.kind = tkChar) and (right.kind = tkCharRange) then
        TypesAreAssignable := true
    else if (left.kind = tkCharRange) and (right.kind = tkChar) then
        TypesAreAssignable := true
    else if (left.kind in [tkEnum, tkEnumMember]) and (right.kind in [tkEnum, tkEnumMember]) then
    begin
        if (left is TEnumMemberTypeDef) and (right is TEnumMemberTypeDef) then
            TypesAreAssignable := TEnumMemberTypeDef(left).enumSpec = TEnumMemberTypeDef(right).enumSpec
        else if (left is TEnumTypeDef) and (right is TEnumMemberTypeDef) then
            TypesAreAssignable := TEnumTypeDef(left).enumSpec = TEnumMemberTypeDef(right).enumSpec
        else if (left is TEnumMemberTypeDef) and (right is TEnumTypeDef) then
            TypesAreAssignable := TEnumMemberTypeDef(left).enumSpec = TEnumTypeDef(right).enumSpec
        else if (left is TEnumTypeDef) and (right is TEnumTypeDef) then
            TypesAreAssignable := TEnumTypeDef(left).enumSpec = TEnumTypeDef(right).enumSpec;
    end
    else if (left.kind = tkSet) and (right.kind = tkSet) then
    begin
        if (left is TSetTypeDef) and (right is TSetTypeDef) then
        begin
            if (TSetTypeDef(left).typeOfSet = nil) or (TSetTypeDef(right).typeOfSet = nil) then
                TypesAreAssignable := true
            else
                TypesAreAssignable := TypesAreAssignable(TSetTypeDef(left).typeOfSet, TSetTypeDef(right).typeOfSet, errorMessage);
        end
        else
            TypesAreAssignable := true;
    end
    else if (left.kind = tkArray) and (right.kind = tkArray) then
    begin
        if (left is TArrayTypeDef) and (right is TArrayTypeDef) then
        begin
            if (TArrayTypeDef(left).typeOfValues = nil) or (TArrayTypeDef(right).typeOfValues = nil) then
                TypesAreAssignable := true
            else
                TypesAreAssignable := TypesAreAssignable(TArrayTypeDef(left).typeOfValues, TArrayTypeDef(right).typeOfValues, errorMessage);
        end
        else
            TypesAreAssignable := true;
    end;

    if (left.kind = tkString) and (right.kind = tkChar) then
        TypesAreAssignable := true
    else if (left.kind = tkPointer) and (left is TPointerTypeDef) and (TPointerTypeDef(left).pointerToType <> nil) and (TPointerTypeDef(left).pointerToType.kind = tkChar) and (right.kind = tkString) then
        TypesAreAssignable := true;

    // TODO: functions

    if not TypesAreAssignable then
    begin
        if (left.kind = tkSet) and (right.kind = tkSet) then
            errorMessage := 'base types of sets are not compatible: ' + errorMessage
        else if (left.kind in [tkEnum, tkEnumMember]) and (right.kind in [tkEnum, tkEnumMember]) then
            errorMessage := 'different enums!'
        else if (ord(left.kind) >= 0) and (ord(left.kind) < NUM_OF_TYPE_KINDS) and (ord(right.kind) >= 0) and (ord(right.kind) < NUM_OF_TYPE_KINDS) then
            errorMessage := 'expected ' + TypeKindStr[ord(left.kind)] + ' or assignment-compatible, but found ' + TypeKindStr[ord(right.kind)] + '!'
        else
            errorMessage := 'types are not assignment-compatible!';
    end;
end;

function HaveSameSignature(a, b: TTypeDef): boolean;
var
    pa, pb: TParameterList;
    i: integer;
begin
    if (a = nil) or (b = nil) then
        exit(false);
    if not (a.kind in [tkProcedure, tkFunction]) or not (b.kind in [tkProcedure, tkFunction]) then
        exit(false);

    if not (a is TRoutineTypeDef) or not (b is TRoutineTypeDef) then
        exit(false);

    if TRoutineTypeDef(a).parameters = TRoutineTypeDef(b).parameters then
        exit(true);

    if (TRoutineTypeDef(a).parameters = nil) or (TRoutineTypeDef(b).parameters = nil) then
        exit(false);

    pa := TParameterList(TRoutineTypeDef(a).parameters);
    pb := TParameterList(TRoutineTypeDef(b).parameters);
    if pa.count <> pb.count then
        exit(false);

    for i := 0 to pa.count - 1 do
    begin
        if pa.items[i].kind <> pb.items[i].kind then
            exit(false);
        if pa.items[i].typeDef <> pb.items[i].typeDef then
            exit(false);
    end;

    HaveSameSignature := true;
end;

initialization
    TypesList := TFPHashList.Create;

    unknownType := TPrimitiveTypeDef.Create(tkUnknown, 0);

    byteType := TIntegerTypeDef.Create(1, false, 0, 255);
    shortintType := TIntegerTypeDef.Create(1, true, -128, 127);
    wordType := TIntegerTypeDef.Create(2, false, 0, 65535);
    smallintType := TIntegerTypeDef.Create(2, true, -32768, 32767);
    longwordType := TIntegerTypeDef.Create(4, false, 0, 4294967295);
    longintType := TIntegerTypeDef.Create(4, true, -2147483648, 2147483647);
    qwordType := TIntegerTypeDef.Create(8, false, 0, 0);
    int64Type := TIntegerTypeDef.Create(8, true, 0, 0);

    booleanType := TPrimitiveTypeDef.Create(tkBoolean, 1);
    boolean16Type := TPrimitiveTypeDef.Create(tkBoolean, 2);
    boolean32Type := TPrimitiveTypeDef.Create(tkBoolean, 4);
    boolean64Type := TPrimitiveTypeDef.Create(tkBoolean, 8);

    charType := TPrimitiveTypeDef.Create(tkChar, 1);
    realType := TPrimitiveTypeDef.Create(tkReal, 4);
    singleType := TPrimitiveTypeDef.Create(tkReal, 4);
    doubleType := TPrimitiveTypeDef.Create(tkReal, 4);
    extendedType := TPrimitiveTypeDef.Create(tkReal, 4);
    compType := TPrimitiveTypeDef.Create(tkReal, 8);
    currencyType := TPrimitiveTypeDef.Create(tkReal, 8);

    pointer32Type := TPointerTypeDef.Create(false, nil, 4);
    pointer64Type := TPointerTypeDef.Create(false, nil, 8);
    pcharType := TPointerTypeDef.Create(true, charType, 8);

    shortstringType := TPrimitiveTypeDef.Create(tkString, 256);
    ansiString32Type := TPrimitiveTypeDef.Create(tkString, 4);
    ansiString64Type := TPrimitiveTypeDef.Create(tkString, 8);

    voidProcedureType := TRoutineTypeDef.Create(tkProcedure, TParameterList.Create, nil, nil);

finalization
    TypesList.Free;
end.
