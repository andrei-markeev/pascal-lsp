program Subranges;

const
    MIN = 0;
    MAX = 10000;
    CHAR_S = 'S';
    CHAR_Y = 'Y';

type
    myRangeType = 'ab'..'z';
    myEnumType = (apple, orange, banana);
    myEnumType2 = (bicycle, bike, car, truck);

var
    intConstWithEnum: MIN..orange;
    charWithConstInt: 'a'..MAX;
    constIntWithConstChar: MIN..CHAR_S;
    typeSubrange: myRangeType;
    minusWithConstChar: -CHAR_S..CHAR_Y;
    minusWithConstChar2: CHAR_S..-CHAR_Y;
    minusWithChar: -'a'..CHAR_S;
    minusWithChar2: -'0'..-'9';
    minusWithChar3: -'a'..'d';
    minusWithChar4: 'd'..-'z';
    goodOne: 5..&123;
    incompatibleEnums: orange..truck;

begin
end.