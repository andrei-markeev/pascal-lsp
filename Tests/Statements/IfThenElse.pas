program IfThenElse;

var
    a, b: integer;
    s: string;
    fruits: (bananas, oranges, apples, grapes);
begin

    { --- good ones --- }

    a := 1;
    if a <> 1 then
        a := a + 1
    else
        b := 10 + 20 + a * 2;
    
    fruits := oranges;

    s := 'this is bananas';
    if fruits > bananas then
        s := 'not bananas but more than bananas!';

    { --- incomplete --- }

    if

    if a > b then
        s := 'no way';
    
    if a

    b := 100;

    if a then
    begin
        s := 'here we go';

end.