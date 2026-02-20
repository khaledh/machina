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

## Package as VSIX

From `tooling/vscode`:

1. `npm run package:vsix`
2. VSIX artifact is written to `tooling/vscode/dist/` (for example `machina-vscode-0.0.1.vsix`)

Use `npm run package:vsix:clean` to clear previous VSIX artifacts before packaging.

## Install VSIX

### VS Code

1. Open Extensions view (`Cmd+Shift+X` / `Ctrl+Shift+X`)
2. Click `...` menu in the top-right
3. Choose `Install from VSIX...`
4. Select the generated file under `tooling/vscode/dist/`

### Cursor

1. Open Extensions view
2. Open `...` menu
3. Choose `Install from VSIX...`
4. Select the generated file under `tooling/vscode/dist/`

## Language server settings

- `machina.languageServer.path`: path to `machina-lsp` (optional)
- `machina.languageServer.args`: extra arguments for `machina-lsp`
- `machina.languageServer.experimentalFeatures`: list of experimental features to enable in LSP (for example `["typestate"]`)

## Troubleshooting

- `spawn machina-lsp ENOENT`:
  set `machina.languageServer.path` to the absolute `machina-lsp` binary path.
  A common local path is `target/debug/machina-lsp` in the Machina repo.
- Server starts but diagnostics are missing:
  run `cargo build -p machina-lsp` and restart the language server (`Machina: Restart Language Server`).
- Need to pass feature flags to the server:
  set `machina.languageServer.experimentalFeatures`, for example:
  `["typestate"]`.
