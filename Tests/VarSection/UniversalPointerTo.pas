program UniversalPointerTo;

{$mode universalpascal}

type
    TIntArray = array[1..10] of integer;
    PIntArray = pointer to TIntArray;
    TNode = record
        value: integer;
        next: pointer to TNode;
    end;

var
    pArr: PIntArray;
    pNode: pointer to TNode;
    val: integer;

begin
    val := pArr[1];
    val := pNode.value;
    pNode.value := 10;
end.
