program FileTypes;

type
    TRec = record
        a: integer;
        b: string;
    end;

var
    t: text;
    tf: textfile;
    fInt: file of integer;
    fRec: file of TRec;
    uFile: file;
    buf: array[1..100] of byte;
    readCount: integer;
begin
    Assign(t, 'input.txt');
    AssignFile(tf, 'output.txt');
    Reset(t);
    Reset(uFile, 512);
    Rewrite(tf);
    Rewrite(fInt);

    if not Eof(t) then
        WriteLn;

    if not Eoln(t) then
        WriteLn;

    Seek(fInt, 10);
    WriteLn(FilePos(fInt));
    WriteLn(FileSize(fInt));
    WriteLn(IOResult);

    BlockRead(uFile, buf, 100, readCount);
    BlockWrite(uFile, buf, 100);

    Flush(t);
    Truncate(fInt);
    Rename(t, 'new.txt');
    Erase(t);

    Close(t);
    CloseFile(tf);
end.
