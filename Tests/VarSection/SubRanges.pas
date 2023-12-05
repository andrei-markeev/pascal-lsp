program Subranges;

const
    MIN = 0;
    MAX = 10000;
    CHAR_S = 'S';
    CHAR_Y = 'Y';

type
    myRangeType = 10..20;
    myEnumType = (apple, orange, banana);

var
    constSubrange: MIN..MAX;
    mixedConstSubrange1: 1..MAX;
    mixedConstSubrange2: MIN..255;
    typeSubrange: myRangeType;
    numberSubrange: 0..200;
    signedNumberRange: -100..100;
    negativeNumberRange: -10..-1;
    positiveNumberRange: +1..+10;
    enumSubrange: orange..banana;
    myLongInt: $80000000..$7fffffff;
    charSubrange: '1'..'9';
    constCharSubrange: CHAR_S..CHAR_Y;
    mixedConstCharSubrange1: 'd'..CHAR_S;
    mixedConstCharSubrange2: CHAR_Y..'z';

begin
end.