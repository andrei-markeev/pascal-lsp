program SsocketsTest;

{$mode objfpc}
{$longstrings on}

uses sysutils, classes, ssockets;

type
    TLspApp = class
    public
        procedure OnConnect(Sender: TObject; Data: TSocketStream);
    end;

procedure TLspApp.OnConnect(Sender: TObject; Data: TSocketStream);
begin
    if Data <> nil then
        Data.Close;
end;

var
    App: TLspApp;
    Server: TInetServer;
    SockStream: TSocketStream;
    InetSock: TInetSocket;
    UnixServer: TUnixServer;
    UnixSock: TUnixSocket;
    Err: ESocketError;
    H: TSocketHandle;
begin
    App := TLspApp.Create;
    Server := TInetServer.Create(8080);
    Server.OnConnect := @App.OnConnect;
    Server.StartAccepting;
    Server.StopAccepting;
    H := Server.Socket;

    UnixServer := TUnixServer.Create('/tmp/test.sock');
    UnixServer.StartAccepting;

    SockStream := Server.Accept;
    if SockStream <> nil then
    begin
        H := SockStream.Handle;
        SockStream.Close;
    end;

    InetSock := TInetSocket.Create('127.0.0.1', 8080);
    UnixSock := TUnixSocket.Create('/tmp/test.sock');

    Server.Free;
    UnixServer.Free;
    App.Free;
end.
