# Machina VS Code/Cursor Extension (POC)

This folder contains the VS Code/Cursor extension scaffold for Machina.

## Features

- **Syntax Highlighting**: Full TextMate grammar for Machina with support for all language features (keywords, attributes, literals, types, operators, etc.)
- **Language Server Protocol (LSP)**: Integration with `machina-lsp` for code intelligence
- **Editor Support**: Auto-closing pairs, bracket matching, comment toggling, and smart indentation

## Local development

1. `cd tooling/vscode`
2. `npm install`
3. `npm run build`
4. Open `tooling/vscode` in VS Code (or Cursor)
5. Start `Run Machina Extension` from the debugger

The extension activates on `.mc` files and exposes:

- **Syntax highlighting** for all Machina language constructs
- `Machina: Show Output`
- `Machina: Ping`
- `Machina: Restart Language Server`

See [SYNTAX-HIGHLIGHTING.md](./SYNTAX-HIGHLIGHTING.md) for details on the syntax highlighting features.

## Language server settings

- `machina.languageServer.path`: path to `machina-lsp` (optional)
- `machina.languageServer.args`: extra arguments for `machina-lsp`
- `machina.languageServer.experimentalFeatures`: list of experimental features to enable in LSP (for example `["typestate"]`)
