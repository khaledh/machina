import * as vscode from "vscode";

const OUTPUT_CHANNEL_NAME = "Machina";

export function activate(context: vscode.ExtensionContext): void {
  const output = vscode.window.createOutputChannel(OUTPUT_CHANNEL_NAME);
  output.appendLine("Machina extension activated.");

  const showOutput = vscode.commands.registerCommand(
    "machina.showOutput",
    () => {
      output.show(true);
      output.appendLine("Showing output channel.");
    }
  );

  const ping = vscode.commands.registerCommand("machina.ping", () => {
    output.appendLine("Ping command invoked.");
    void vscode.window.showInformationMessage("Machina extension is active.");
  });

  context.subscriptions.push(output, showOutput, ping);
}

export function deactivate(): void {
  // No background resources yet; kept for future LSP client lifecycle wiring.
}
