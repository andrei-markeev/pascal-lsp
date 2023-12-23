program SetsIntersection;

type
    TFruits = (banana, apple, orange, kiwi, plum);
    TVegetables = (cucumber, carrot, potato, turnip, cabbage);
var
    myFruits: set of TFruits;
    hisFruits: set of TFruits;
    vegetables: set of TVegetables;
    result: set of TFruits;
    i: integer;
begin

    i := 1;

    myFruits := [banana, kiwi];
    hisFruits := [apple, orange, kiwi];

    result := myFruits * hisFruits;
    result := myFruits * [orange];
    result := [orange] * hisFruits;
    result := [apple, kiwi] * [];
    result := [] * [banana];
    result := [apple, kiwi] * [apple, banana] * [plum];

    result := vegetables * [cabbage];
    result := vegetables * [orange];
    result := vegetables * hisFruits;
    result := vegetables * [];

    result := [cabbage] * vegetables;
    result := [orange] * vegetables;
    result := hisFruits * vegetables;
    result := [] * vegetables;

    result := 'test' * [plum];
    result := [orange] *

end.