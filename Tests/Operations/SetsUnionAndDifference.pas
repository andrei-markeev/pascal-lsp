program SetsUnionAndDifference;

type
    TFruits = (banana, apple, orange, kiwi);
    TVegetables = (cucumber, carrot, potato, turnip, cabbage);
var
    myFruits: set of TFruits;
    hisFruits: set of TFruits;
    result: set of TFruits;
    i: integer;
begin

    i := 1;

    myFruits := [banana, kiwi];
    hisFruits := [apple, orange, kiwi];

    result := myFruits + hisFruits;
    result := myFruits - hisFruits;
    result := myFruits + [orange];
    result := [apple, kiwi] + [];
    result := [] + [banana];
    result := [apple, kiwi] - [apple, banana];

    result := [apple, kiwi] + [cabbage];
    result := [potato, carrot] - [orange];

    result := myFruits + orange;
    result := myFruits - banana;

    result := myFruits + 1;
    result := myFruits - [1];
    result := 'test' + myFruits;

end.