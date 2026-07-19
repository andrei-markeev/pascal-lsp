unit InterfaceRoutineDecl;

interface

function FindSymbol(name: string): integer; overload;
function FindSymbol(id: integer): integer; overload;

implementation

function Helper: integer;
begin
    Helper := FindSymbol('test');
end;

function FindSymbol(name: string): integer;
begin
    FindSymbol := 1;
end;

function FindSymbol(id: integer): integer;
begin
    FindSymbol := id;
end;

end.
