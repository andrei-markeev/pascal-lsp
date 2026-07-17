# Developing and Debugging Pascal LSP

This document describes how to build, run, and debug the Pascal LSP server and its VS Code client extension.

---

## 1. Compiling the LSP Server

The LSP server is implemented in Free Pascal (`LspServer.pas`). You can compile it to `build/LspServer.exe` by running the [build-lsp.sh](file:///c:/my/Projects/pascal-lsp/build-lsp.sh) helper script:

```bash
./build-lsp.sh
```

## 2. Setting Up the VS Code Extension

The extension client is written in TypeScript and resides in the [vscode-extension](file:///c:/my/Projects/pascal-lsp/vscode-extension) directory.

### Initial Setup & Dependency Installation
Open a terminal in the `vscode-extension` directory and run:
```bash
npm install
```

### Building the Extension
To compile the TypeScript source files to JavaScript:
```bash
npm run compile
```

To continuously compile files as they change (useful during development):
```bash
npm run watch
```

---

## 3. Running & Debugging the Extension

To run the extension and test the LSP server integration locally:

1. Open the [vscode-extension](file:///c:/my/Projects/pascal-lsp/vscode-extension) folder in a new VS Code window.
2. Ensure you have compiled the LSP server (e.g. `build/LspServer.exe` exists) and the TypeScript code (`npm run compile`).
3. Press **F5** (or go to **Run and Debug** in the sidebar and choose **Launch Extension**).
4. A new window named **[Extension Development Host]** will open.
5. In this new window, open the main codebase workspace (e.g., `c:\my\Projects\pascal-lsp`).
6. Open any `.pas` file. The extension will activate and launch the LSP server.

### Verifying Features
- **Symbols**: Look at the VS Code **Outline** panel or hit `Ctrl+Shift+O` (`Cmd+Shift+O`) to see the parsed symbols.
- **Diagnostics**: Make a syntax error in a `.pas` file and save. Verify that the syntax error displays in the **Problems** pane and as a red squiggly underline in the editor.

---

## 4. Debugging the LSP Server Executable

If you want to debug the underlying Pascal code of the LSP server itself:
- Since the server is started by VS Code via `--stdio`, you can view stderr output logs by checking the **Debug Console** or redirecting standard error inside the extension launch client config.
- You can also compile the server with debug information (`-gl`) and attach your favorite Pascal debugger (such as GDB inside Lazarus/VS Code) to the running `LspServer.exe` process.
- Alternatively, run `LspServer.exe` on a custom TCP port using `--port=5007` and configure the client extension to connect to it via socket stream for easier profiling.
