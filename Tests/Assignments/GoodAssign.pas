program GoodAssign;

var
    a, b: integer;
    s: string;
    fruits: (bananas, oranges, apples, grapes);
begin
    a := 1;
    a := a + 1;
    b := 10 + 20 + a;
    s := '';
    s := s + 'something else' + #13 + #$0a;
    fruits := oranges;
end.