program BadAssign;

type
    TFruits = (bananas, oranges, apples, grapes);
    TMarks = (markNone, markX, markO);

var
    a, b: integer;
    s: string;
    fruits: TFruits;
    fruitStorage: array [TFruits] of 0..100;
    storageHas: set of TFruits;
    ticTacToe: array [0..2, 0..2] of (markNone, markX, markO);
    dynArr: array of string;

begin
    a := '1';
    b := 10 + '20' + a;
    b := bananas;
    s := markNone;
    s := s + 'something else' + markX + #$0a;

    fruits := markX;
    fruits := dynArr[12];
    fruitStorage[fruits] := '1243';
    fruitStorage['bananas'] := 5;
    fruitStorage[10] := 111;
    fruitStorage[markX] := 12;

    storageHas := 123;
    storageHas := 'wat';
    storageHas := [markX];
    storageHas := ['bananas'];
    storageHas := [1..2];
    storageHas := [oranges, markO];

    a := fruitStorage[markO];
    b := fruitStorage['oranges'];

    ticTacToe['0', 0] := markX;
    ticTacToe[1][oranges] := markO;

    dynArr[0] := 123.45;
    dynArr[1] := ticTacToe[0][1];
end.