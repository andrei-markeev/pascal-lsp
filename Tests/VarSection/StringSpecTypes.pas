unit StringSpecTypes;

interface

type
    TOberonReceiver = record
        funcToken: integer;
        receiverStart: PChar;
        receiverLen: integer;
        selfTypeName: string[32];
    end;

var
    s32: string[32];
    s255: string[255];

implementation

end.
