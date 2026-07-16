# Pascal LSP Codebase Context & Architecture

This document provides a high-level guide to the codebase architecture, design principles, type system, scoping mechanism, and testing strategy of `pascal-lsp`.

## 1. High-Level Architecture & Design Principles
- **No External Dependencies**: The parser is written from scratch in Object Pascal using FPC (Free Pascal Compiler) without depending on Lazarus Code Tools or other heavy external libraries. It is designed for maximum speed and minimal memory footprint.
- **OO Recursive Descent / In-Token Parsing**: Parsing logic is directly embedded in the constructor of `TToken` subclasses (e.g., `ProgramFile.pas`, `UnitFile.pas`, `Block.pas`, `VarDecl.pas`). Instantiating a token parses its corresponding structure from the `TParserContext` by advancing the cursor, skipping trivia (comments, spaces), and appending the token or its children to the context's global token list.

## 2. Error Recovery & Safety Guidelines
- **Landmark Anchors**: To prevent syntax errors from causing cascading failures, the parser uses a synchronization recovery mechanism. Landmarked keywords (like `rwConst`, `rwVar`, `rwBegin`, `rwSemiColon`) are registered as active anchors.
- **Resynchronization (Anchors)**: If parsing fails or hits an invalid construct, it calls `SkipUntilAnchor` in `Anchors.pas` to skip invalid tokens (wrapping them in tags with `SKIPPED="true"`) until it matches an active anchor, gracefully recovering and continuing.
- **Missing Elements**: When an expected keyword, identifier, or symbol is missing but non-fatal, instantiate it with `state := tsMissing` and set its position before any skipped trivia (using `ctx.GetCursorBeforeTrivia` or similar). This outputs a `<TokenName MISSING="..." />` node in the XML. Do not consume subsequent valid tokens as missing ones.

## 3. Scopes and Symbols
- **Lexical Scoping**: Lexical scopes are instances of `TScope` managed in a global `ScopesList` in `Scopes.pas`. A scope is registered when entering constructs like blocks (`Block.pas`) or `with` statements (`WithStatement.pas`).
- **Scope Resolution**: The scope at any given cursor offset is determined by searching `ScopesList` backwards for the first scope whose boundaries encompass the offset (`FindScope` in `Scopes.pas`).
- **Flat Symbol Tables via Prefixes**: To keep lookup fast and avoid deeply nested tables, nested symbols (e.g., class fields, method parameters) are registered in the current scope prefixed by their parent's unique ID/prefix (e.g., `parent.uniquePrefix + symbolName`). Standard lookups climb the `parentScope` chain (`FindSymbol` in `Symbols.pas`).
- **Object/Class Member Scoping**: When a class/object method block is parsed, member declarations of that class/object are registered into the local scope of the block, making them directly resolvable (along with special variables like `Self` and `Result`).

## 4. Predefined & Custom Types
- **Compilation Modes**: The parser detects and sets the compilation mode (e.g., Standard Pascal, Turbo Pascal, Free Pascal/Delphi) via source directives like `{$mode objfpc}`. Predefined types (e.g., `integer`, `ansistring`, `boolean32`) are pre-loaded according to the active mode.
- **Type Information**: Subroutines, variables, and expressions store type specifications (`TTypeDef` variant records in `TypeDefs.pas`). Type assignability and signature compatibility are validated during parsing, flagging type mismatch errors directly in the AST.

## 5. Coding guidelines
- **Minimize Comments**: Code should be self-explanatory and easy to read. Comments are avoided as they easily get outdated. Comments are permitted only for:
  - Documenting TODOs
  - Documenting non-obvious hacks
  - Documenting highly complex algorithms

## 6. Testing Strategy (Snapshot Testing)
- **XML Serialization**: For testing purposes, the internal token stream/AST is serialized by `ParseFile.pas` into an inline-annotated XML representation. This format interleaves structural XML tags directly with the original source code offsets (e.g., `<RW>program</RW> <SymbDecl>MyProgram</SymbDecl>`) to represent the token stream.
- **Snapshot Diffing**: Each syntax test in the `Tests` directory consists of a `.pas` source file and a `.shouldbe.xml` expected output file. Running the test runner compiles the parser, generates the `.out.xml` files, and compares them against the expected `.shouldbe.xml` snapshots using `git diff --no-index`. Any variance in structure, scope resolution, or errors shows up as a failed diff.
- **Failure / Recovery Testing**: Every new parser feature MUST include both a successful parsing test case (e.g. `Heritage.pas`) and a dedicated error recovery test case (e.g. `BadHeritage.pas`) containing syntax errors, missing elements, and unexpected tokens to verify correct recovery without cascading errors.
- **Use ./runtests.sh**: use ./runtests.sh for compiling and running tests (it does both).

---
### Key Files:
- `ParserContext.pas`: Manages cursor, trivia skipping, compilation modes, and token stream.
- `Token.pas`: Base class for AST representation.
- `Scopes.pas`: Implements block-level scopes.
- `Symbols.pas`: Handles symbol registration, references, and name resolution.
- `Anchors.pas`: Implements token categorization and error recovery.
- `TypeDefs.pas` (in `Types/` directory): Definitions for Pascal type hierarchies.
- `ParseFile.pas`: Command-line entry point converting Pascal to XML.
- `Tests/Makefile`: Test execution and snapshot verification setup.
