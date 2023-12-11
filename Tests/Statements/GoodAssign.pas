program GoodAssign;

type
    TFruits = (bananas, oranges, apples, grapes);

var
    a, b: integer;
    s: string;
    fruits: TFruits;
    fruitStorage: array [TFruits] of 0..100;
    ticTacToe: array [0..2, 0..2] of (markX, markO);
    dynArr: array of string;

begin
    a := 1;
    a := a + 1;
    b := 10 + 20 + a;
    s := '';
    s := s + 'something else' + #13 + #$0a;

    fruits := oranges;
    fruitStorage[fruits] := 10;
    fruitStorage[bananas] := 5;

    a := fruitStorage[apples];
    b := fruitStorage[fruits];

    ticTacToe[0, 0] := markX;
    ticTacToe[1][1] := markO;

    dynArr[0] := 'hello';
    dynArr[1] := 'world';
    s := dynArr[12];
end.