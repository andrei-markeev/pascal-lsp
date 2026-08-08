unit SsocketsUnit;

{$mode objfpc}
{$longstrings on}

interface

uses
    ParserContext, SystemUnit, TypeDef, TypeDefs;

type
    TSsocketsUnit = class(TSystemUnit)
    private
        classType_ESocketError: TTypeDef;
        classType_TSocketStream: TTypeDef;
        classType_TInetSocket: TTypeDef;
        classType_TUnixSocket: TTypeDef;
        classType_TSocketServer: TTypeDef;
        classType_TInetServer: TTypeDef;
        classType_TUnixServer: TTypeDef;
        classType_TSocketHandler: TTypeDef;

        procType_ConnectEvent: TTypeDef;
        procType_FilterEvent: TTypeDef;

        func_Create_ESocketError: TTypeDef;
        func_Create_TSocketStream: TTypeDef;
        func_Create_TSocketStream_HostPort: TTypeDef;
        func_Create_TInetSocket: TTypeDef;
        func_Create_TUnixSocket: TTypeDef;
        func_Create_TSocketServer: TTypeDef;
        func_Create_TInetServer: TTypeDef;
        func_Create_TInetServer_HostPort: TTypeDef;
        func_Create_TUnixServer: TTypeDef;
        func_Create_TSocketHandler: TTypeDef;
        func_Accept_TSocketStream: TTypeDef;
    protected
        procedure InitTypes; override;
    public
        destructor Destroy; override;
        procedure Load(ctx: TParserContext); override;
    end;

implementation

uses
    Symbols, CompilationMode, Parameters, ClassTypeDef, ClassesUnit, SystemUnits;

destructor TSsocketsUnit.Destroy;
begin
    if loaded then
    begin
        classType_ESocketError.Free;
        classType_TSocketStream.Free;
        classType_TInetSocket.Free;
        classType_TUnixSocket.Free;
        classType_TSocketServer.Free;
        classType_TInetServer.Free;
        classType_TUnixServer.Free;
        classType_TSocketHandler.Free;

        procType_ConnectEvent.Free;
        procType_FilterEvent.Free;

        func_Create_ESocketError.Free;
        func_Create_TSocketStream.Free;
        func_Create_TSocketStream_HostPort.Free;
        func_Create_TInetSocket.Free;
        func_Create_TUnixSocket.Free;
        func_Create_TSocketServer.Free;
        func_Create_TInetServer.Free;
        func_Create_TInetServer_HostPort.Free;
        func_Create_TUnixServer.Free;
        func_Create_TSocketHandler.Free;
        func_Accept_TSocketStream.Free;
    end;
    inherited Destroy;
end;

procedure TSsocketsUnit.InitTypes;
begin
    // ESocketError
    classType_ESocketError := TClassTypeDef.Create;
    TClassTypeDef(classType_ESocketError).parentClass := classType_TObject;

    func_Create_ESocketError := CreateOneParamFunctionType('msg', ansiString64Type, classType_ESocketError);
    TClassTypeDef(classType_ESocketError).AddMember('Message', ansiString64Type);
    TClassTypeDef(classType_ESocketError).AddMember('Code', longintType);
    TClassTypeDef(classType_ESocketError).AddMember('Create', func_Create_ESocketError);
    TClassTypeDef(classType_ESocketError).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_ESocketError).AddMember('Free', voidProcedureType);

    // TSocketStream
    classType_TSocketStream := TClassTypeDef.Create;
    TClassTypeDef(classType_TSocketStream).parentClass := classesMock.classType_THandleStream;

    func_Create_TSocketStream := CreateOneParamFunctionType('ahandle', longintType, classType_TSocketStream);
    func_Create_TSocketStream_HostPort := CreateTwoParamFunctionType('ahost', ansiString64Type, 'aport', longintType, classType_TSocketStream);

    TClassTypeDef(classType_TSocketStream).AddMember('Handle', longintType);
    TClassTypeDef(classType_TSocketStream).AddMember('Host', ansiString64Type);
    TClassTypeDef(classType_TSocketStream).AddMember('Port', longintType);
    TClassTypeDef(classType_TSocketStream).AddMember('RemoteAddress', ansiString64Type);
    TClassTypeDef(classType_TSocketStream).AddMember('Create', func_Create_TSocketStream);
    TClassTypeDef(classType_TSocketStream).AddMember('Close', voidProcedureType);
    TClassTypeDef(classType_TSocketStream).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TSocketStream).AddMember('Free', voidProcedureType);

    // TInetSocket
    classType_TInetSocket := TClassTypeDef.Create;
    TClassTypeDef(classType_TInetSocket).parentClass := classType_TSocketStream;

    func_Create_TInetSocket := CreateTwoParamFunctionType('ahost', ansiString64Type, 'aport', longintType, classType_TInetSocket);
    TClassTypeDef(classType_TInetSocket).AddMember('Host', ansiString64Type);
    TClassTypeDef(classType_TInetSocket).AddMember('Port', longintType);
    TClassTypeDef(classType_TInetSocket).AddMember('Create', func_Create_TInetSocket);
    TClassTypeDef(classType_TInetSocket).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TInetSocket).AddMember('Free', voidProcedureType);

    // TUnixSocket
    classType_TUnixSocket := TClassTypeDef.Create;
    TClassTypeDef(classType_TUnixSocket).parentClass := classType_TSocketStream;

    func_Create_TUnixSocket := CreateOneParamFunctionType('afilename', ansiString64Type, classType_TUnixSocket);
    TClassTypeDef(classType_TUnixSocket).AddMember('FileName', ansiString64Type);
    TClassTypeDef(classType_TUnixSocket).AddMember('Create', func_Create_TUnixSocket);
    TClassTypeDef(classType_TUnixSocket).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TUnixSocket).AddMember('Free', voidProcedureType);

    // Callbacks
    procType_ConnectEvent := CreateTwoParamProcedureType('sender', classType_TObject, 'data', classType_TSocketStream);
    procType_FilterEvent := CreateTwoParamProcedureType('sender', classType_TObject, 'data', classType_TSocketStream);

    // TSocketServer
    classType_TSocketServer := TClassTypeDef.Create;
    TClassTypeDef(classType_TSocketServer).parentClass := classType_TObject;

    func_Create_TSocketServer := CreateOneParamFunctionType('asocket', longintType, classType_TSocketServer);
    func_Accept_TSocketStream := CreateFunctionType(TParameterList.Create, classType_TSocketStream);

    TClassTypeDef(classType_TSocketServer).AddMember('Socket', longintType);
    TClassTypeDef(classType_TSocketServer).AddMember('OnConnect', procType_ConnectEvent);
    TClassTypeDef(classType_TSocketServer).AddMember('OnFilter', procType_FilterEvent);
    TClassTypeDef(classType_TSocketServer).AddMember('NonBlocking', booleanType);
    TClassTypeDef(classType_TSocketServer).AddMember('Active', booleanType);
    TClassTypeDef(classType_TSocketServer).AddMember('QueueSize', longintType);
    TClassTypeDef(classType_TSocketServer).AddMember('MaxConnections', longintType);
    TClassTypeDef(classType_TSocketServer).AddMember('AcceptBacklog', longintType);
    TClassTypeDef(classType_TSocketServer).AddMember('Create', func_Create_TSocketServer);
    TClassTypeDef(classType_TSocketServer).AddMember('StartAccepting', voidProcedureType);
    TClassTypeDef(classType_TSocketServer).AddMember('StopAccepting', voidProcedureType);
    TClassTypeDef(classType_TSocketServer).AddMember('Accept', func_Accept_TSocketStream);
    TClassTypeDef(classType_TSocketServer).AddMember('Close', voidProcedureType);
    TClassTypeDef(classType_TSocketServer).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TSocketServer).AddMember('Free', voidProcedureType);

    // TInetServer
    classType_TInetServer := TClassTypeDef.Create;
    TClassTypeDef(classType_TInetServer).parentClass := classType_TSocketServer;

    func_Create_TInetServer := CreateOneParamFunctionType('aport', longintType, classType_TInetServer);
    func_Create_TInetServer_HostPort := CreateTwoParamFunctionType('ahost', ansiString64Type, 'aport', longintType, classType_TInetServer);

    TClassTypeDef(classType_TInetServer).AddMember('Host', ansiString64Type);
    TClassTypeDef(classType_TInetServer).AddMember('Port', longintType);
    TClassTypeDef(classType_TInetServer).AddMember('Create', func_Create_TInetServer);
    TClassTypeDef(classType_TInetServer).AddMember('StartAccepting', voidProcedureType);
    TClassTypeDef(classType_TInetServer).AddMember('StopAccepting', voidProcedureType);
    TClassTypeDef(classType_TInetServer).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TInetServer).AddMember('Free', voidProcedureType);

    // TUnixServer
    classType_TUnixServer := TClassTypeDef.Create;
    TClassTypeDef(classType_TUnixServer).parentClass := classType_TSocketServer;

    func_Create_TUnixServer := CreateOneParamFunctionType('afilename', ansiString64Type, classType_TUnixServer);

    TClassTypeDef(classType_TUnixServer).AddMember('FileName', ansiString64Type);
    TClassTypeDef(classType_TUnixServer).AddMember('Create', func_Create_TUnixServer);
    TClassTypeDef(classType_TUnixServer).AddMember('StartAccepting', voidProcedureType);
    TClassTypeDef(classType_TUnixServer).AddMember('StopAccepting', voidProcedureType);
    TClassTypeDef(classType_TUnixServer).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TUnixServer).AddMember('Free', voidProcedureType);

    // TSocketHandler
    classType_TSocketHandler := TClassTypeDef.Create;
    TClassTypeDef(classType_TSocketHandler).parentClass := classType_TObject;

    func_Create_TSocketHandler := CreateFunctionType(TParameterList.Create, classType_TSocketHandler);

    TClassTypeDef(classType_TSocketHandler).AddMember('Create', func_Create_TSocketHandler);
    TClassTypeDef(classType_TSocketHandler).AddMember('Close', voidProcedureType);
    TClassTypeDef(classType_TSocketHandler).AddMember('Accept', func_Accept_TSocketStream);
    TClassTypeDef(classType_TSocketHandler).AddMember('Destroy', voidProcedureType);
    TClassTypeDef(classType_TSocketHandler).AddMember('Free', voidProcedureType);
end;

procedure TSsocketsUnit.Load(ctx: TParserContext);
begin
    classesMock.Load(ctx);
    inherited Load(ctx);
    if ctx.mode in [cmFreePascal, cmObjectFreePascal] then
    begin
        RegisterSymbolByName('TSocketHandle', nil, skTypeName, longintType, ctx.Cursor);
        RegisterSymbolByName('ESocketError', nil, skTypeName, classType_ESocketError, ctx.Cursor);
        RegisterSymbolByName('TSocketStream', nil, skTypeName, classType_TSocketStream, ctx.Cursor);
        RegisterSymbolByName('TInetSocket', nil, skTypeName, classType_TInetSocket, ctx.Cursor);
        RegisterSymbolByName('TUnixSocket', nil, skTypeName, classType_TUnixSocket, ctx.Cursor);
        RegisterSymbolByName('TSocketServer', nil, skTypeName, classType_TSocketServer, ctx.Cursor);
        RegisterSymbolByName('TInetServer', nil, skTypeName, classType_TInetServer, ctx.Cursor);
        RegisterSymbolByName('TUnixServer', nil, skTypeName, classType_TUnixServer, ctx.Cursor);
        RegisterSymbolByName('TSocketHandler', nil, skTypeName, classType_TSocketHandler, ctx.Cursor);
        RegisterSymbolByName('TConnectEvent', nil, skTypeName, procType_ConnectEvent, ctx.Cursor);
        RegisterSymbolByName('TFilterEvent', nil, skTypeName, procType_FilterEvent, ctx.Cursor);
    end;
end;

end.
