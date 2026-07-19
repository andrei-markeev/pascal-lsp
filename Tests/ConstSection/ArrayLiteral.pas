{$mode objfpc}
unit ArrayLiteral;

interface

const
    SymbolKindStr: array [0..6] of shortstring = (
        '', 'constant', 'typed constant', 'type', 'variable', 'procedure', 'function'
    );
    Numbers: array [1..3] of integer = (1, 2, 3);

implementation

end.
