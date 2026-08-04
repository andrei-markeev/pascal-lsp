program BadVarDefault;

{$mode macpas}
var
    badModeVar: integer = 5;

{$mode objfpc}
var
    missingVal: integer = ;
    typeMismatch: integer = 'hello';

begin
end.
