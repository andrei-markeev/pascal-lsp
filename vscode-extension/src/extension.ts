import * as path from 'path';
import * as vscode from 'vscode';
import {
    LanguageClient,
    LanguageClientOptions,
    ServerOptions
} from 'vscode-languageclient/node';

let client: LanguageClient;

export function activate(context: vscode.ExtensionContext) {
    const config = vscode.workspace.getConfiguration('pascalLsp');
    let serverPath = config.get<string>('serverPath');
    
    if (!serverPath) {
        const workspaceFolders = vscode.workspace.workspaceFolders;
        if (workspaceFolders && workspaceFolders.length > 0) {
            serverPath = path.join(workspaceFolders[0].uri.fsPath, 'build', 'LspServer.exe');
        } else {
            serverPath = context.asAbsolutePath(path.join('build', 'LspServer.exe'));
        }
    }

    console.log(`Pascal LSP client: starting server from ${serverPath}`);

    const configuredPaths = config.get<string[]>('configuredPaths') || [];
    const readLpi = config.get<boolean>('readLpi', true);
    const readDproj = config.get<boolean>('readDproj', true);
    const scanProjectFolders = config.get<boolean>('scanProjectFolders', true);

    const serverOptions: ServerOptions = {
        run: { command: serverPath, args: ['--stdio'] },
        debug: { command: serverPath, args: ['--stdio'] }
    };

    const clientOptions: LanguageClientOptions = {
        documentSelector: [{ scheme: 'file', language: 'pascal' }],
        synchronize: {
            fileEvents: vscode.workspace.createFileSystemWatcher('**/*.pas')
        },
        initializationOptions: {
            configuredPaths: configuredPaths,
            readLpi: readLpi,
            readDproj: readDproj,
            scanProjectFolders: scanProjectFolders
        }
    };

    client = new LanguageClient(
        'pascalLsp',
        'Pascal Language Server',
        serverOptions,
        clientOptions
    );

    client.start();
}

export function deactivate(): Thenable<void> | undefined {
    if (!client) {
        return undefined;
    }
    return client.stop();
}
