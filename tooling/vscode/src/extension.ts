import * as fs from "node:fs";
import * as path from "node:path";
import * as vscode from "vscode";
import {
  CloseAction,
  ErrorAction,
  LanguageClient,
  LanguageClientOptions,
  ServerOptions,
  TransportKind,
} from "vscode-languageclient/node";

const OUTPUT_CHANNEL_NAME = "Machina";

let output: vscode.OutputChannel | undefined;
let client: LanguageClient | undefined;

export function activate(context: vscode.ExtensionContext): void {
  output = vscode.window.createOutputChannel(OUTPUT_CHANNEL_NAME);
  output.appendLine("Machina extension activated.");

  const showOutput = vscode.commands.registerCommand(
    "machina.showOutput",
    () => {
      output?.show(true);
      output?.appendLine("Showing output channel.");
    }
  );

  const ping = vscode.commands.registerCommand("machina.ping", () => {
    output?.appendLine("Ping command invoked.");
    void vscode.window.showInformationMessage("Machina extension is active.");
  });

  const restartLanguageServer = vscode.commands.registerCommand(
    "machina.restartLanguageServer",
    async () => {
      const restarted = await restartClient(context);
      if (restarted) {
        void vscode.window.showInformationMessage("Machina language server restarted.");
      }
    }
  );

  context.subscriptions.push(output, showOutput, ping, restartLanguageServer);
  void ensureClientStarted(context);
}

export async function deactivate(): Promise<void> {
  await stopClient();
}

async function ensureClientStarted(context: vscode.ExtensionContext): Promise<boolean> {
  if (client) {
    return true;
  }

  const { command, args } = resolveServerCommand(context);
  output?.appendLine(`Starting language server: ${command} ${args.join(" ")}`.trim());

  const serverOptions: ServerOptions = {
    command,
    args,
    transport: TransportKind.stdio,
  };

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "machina" }],
    outputChannel: output,
    synchronize: {
      configurationSection: "machina.languageServer",
    },
    errorHandler: {
      error(error, message, count) {
        output?.appendLine(
          `LSP error (count=${count}, message=${String(message)}): ${String(error)}`
        );
        return { action: ErrorAction.Continue };
      },
      closed() {
        output?.appendLine("LSP connection closed; requesting automatic restart.");
        return { action: CloseAction.Restart };
      },
    },
  };

  client = new LanguageClient(
    "machina-lsp",
    "Machina Language Server",
    serverOptions,
    clientOptions
  );

  try {
    await client.start();
    output?.appendLine("Machina language server started.");
    return true;
  } catch (error) {
    client = undefined;
    output?.appendLine(`Failed to start language server: ${formatError(error)}`);
    output?.show(true);
    void vscode.window.showErrorMessage(
      "Failed to start Machina language server. See Machina output."
    );
    return false;
  }
}

async function restartClient(context: vscode.ExtensionContext): Promise<boolean> {
  await stopClient();
  return ensureClientStarted(context);
}

async function stopClient(): Promise<void> {
  if (!client) {
    return;
  }
  const current = client;
  client = undefined;
  output?.appendLine("Stopping Machina language server.");
  await current.stop();
}

function resolveServerCommand(
  context: vscode.ExtensionContext
): { command: string; args: string[] } {
  const cfg = vscode.workspace.getConfiguration("machina.languageServer");
  const configuredPath = (cfg.get<string>("path") ?? "").trim();
  const configuredArgs = cfg.get<string[]>("args") ?? [];

  if (configuredPath.length > 0) {
    return { command: configuredPath, args: configuredArgs };
  }

  const repoLocalDebug = path.resolve(
    context.extensionPath,
    "..",
    "..",
    "target",
    "debug",
    "machina-lsp"
  );
  if (fs.existsSync(repoLocalDebug)) {
    return { command: repoLocalDebug, args: configuredArgs };
  }

  return { command: "machina-lsp", args: configuredArgs };
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }
  return String(error);
}
