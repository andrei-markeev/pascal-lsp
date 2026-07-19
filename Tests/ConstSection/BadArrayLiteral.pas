{$mode objfpc}
unit BadArrayLiteral;

interface

const
    BadArray1: array [0..2] of shortstring = ( 'one', 'two' ;
    BadArray2: array [1..2] of integer = (1 2);

implementation

end.
