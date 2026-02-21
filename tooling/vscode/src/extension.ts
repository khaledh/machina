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

  const onThemeChange = vscode.window.onDidChangeActiveColorTheme(
    () => void applyDefaultTokenColors()
  );

  context.subscriptions.push(output, showOutput, ping, restartLanguageServer, onThemeChange);
  void applyDefaultTokenColors();
  void ensureClientStarted(context);
}

export async function deactivate(): Promise<void> {
  await stopClient();
}

// Default token color rules for Machina files, tuned per theme kind.
// Scopes are `.machina`-suffixed so they only affect Machina grammar tokens
// and are inert if the extension is later disabled.

type TokenRule = { scope: string | string[]; settings: Record<string, string> };

const DARK_TOKEN_RULES: TokenRule[] = [
  { scope: "comment.line.double-slash.machina", settings: { foreground: "#6A9955" } },
  { scope: "keyword.control.machina", settings: { foreground: "#C586C0" } },
  { scope: "keyword.other.machina", settings: { foreground: "#569CD6" } },
  { scope: "storage.modifier.machina", settings: { foreground: "#569CD6" } },
  { scope: "constant.language.boolean.machina", settings: { foreground: "#569CD6" } },
  { scope: "entity.name.tag.machina", settings: { foreground: "#D7BA7D", fontStyle: "italic" } },
  { scope: "entity.name.function.machina", settings: { foreground: "#DCDCAA" } },
  { scope: "entity.name.function.definition.machina", settings: { foreground: "#DCDCAA", fontStyle: "bold" } },
  { scope: "entity.name.type.machina", settings: { foreground: "#78DCE8" } },
  { scope: "support.type.primitive.machina", settings: { foreground: "#4EC9B0" } },
  { scope: "string.quoted.double.machina", settings: { foreground: "#CE9178" } },
  { scope: "string.quoted.single.machina", settings: { foreground: "#CE9178" } },
  { scope: "constant.character.escape.machina", settings: { foreground: "#D7BA7D" } },
  {
    scope: [
      "constant.numeric.decimal.machina",
      "constant.numeric.binary.machina",
      "constant.numeric.octal.machina",
      "constant.numeric.hex.machina",
    ],
    settings: { foreground: "#B5CEA8" },
  },
  { scope: "keyword.operator.arrow.machina", settings: { foreground: "#569CD6" } },
];

const LIGHT_TOKEN_RULES: TokenRule[] = [
  { scope: "comment.line.double-slash.machina", settings: { foreground: "#A0A0A0" } },
  { scope: "keyword.control.machina", settings: { foreground: "#AF00DB" } },
  { scope: "keyword.other.machina", settings: { foreground: "#0000FF" } },
  { scope: "storage.modifier.machina", settings: { foreground: "#0000FF" } },
  { scope: "constant.language.boolean.machina", settings: { foreground: "#0000FF" } },
  { scope: "entity.name.tag.machina", settings: { foreground: "#795E26", fontStyle: "italic" } },
  { scope: "entity.name.function.machina", settings: { foreground: "#000000" } },
  { scope: "entity.name.function.definition.machina", settings: { foreground: "#000000", fontStyle: "bold" } },
  { scope: "entity.name.type.machina", settings: { foreground: "#C73D1A" } },
  { scope: "support.type.primitive.machina", settings: { foreground: "#267F99" } },
  { scope: "string.quoted.double.machina", settings: { foreground: "#008000" } },
  { scope: "string.quoted.single.machina", settings: { foreground: "#008000" } },
  { scope: "constant.character.escape.machina", settings: { foreground: "#EE0000" } },
  {
    scope: [
      "constant.numeric.decimal.machina",
      "constant.numeric.binary.machina",
      "constant.numeric.octal.machina",
      "constant.numeric.hex.machina",
    ],
    settings: { foreground: "#098658" },
  },
  { scope: "keyword.operator.arrow.machina", settings: { foreground: "#0000FF" } },
];

function tokenRulesForThemeKind(): TokenRule[] {
  const kind = vscode.window.activeColorTheme.kind;
  if (kind === vscode.ColorThemeKind.Light || kind === vscode.ColorThemeKind.HighContrastLight) {
    return LIGHT_TOKEN_RULES;
  }
  return DARK_TOKEN_RULES;
}

function isMachinaScope(scope: unknown): boolean {
  if (typeof scope === "string") {
    return scope.includes(".machina");
  }
  if (Array.isArray(scope)) {
    return scope.some((s) => typeof s === "string" && s.includes(".machina"));
  }
  return false;
}

/** Apply Machina token colors matching the active theme kind. On theme
 *  changes the old Machina rules are swapped out for the new set. Skips
 *  entirely if `machina.tokenColors.enabled` is false. */
async function applyDefaultTokenColors(): Promise<void> {
  const enabled = vscode.workspace
    .getConfiguration("machina.tokenColors")
    .get<boolean>("enabled", true);
  if (!enabled) {
    return;
  }

  const editorConfig = vscode.workspace.getConfiguration("editor");
  const existing = editorConfig.get<Record<string, unknown>>("tokenColorCustomizations") ?? {};
  const existingRules = (existing.textMateRules as Array<{ scope: unknown }>) ?? [];

  // Strip any previously-injected Machina rules so we can replace them
  // with the set matching the current theme kind.
  const nonMachinaRules = existingRules.filter((rule) => !isMachinaScope(rule.scope));

  const merged = {
    ...existing,
    textMateRules: [...nonMachinaRules, ...tokenRulesForThemeKind()],
  };
  await editorConfig.update(
    "tokenColorCustomizations",
    merged,
    vscode.ConfigurationTarget.Global
  );
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
  const experimentalFeatures = configuredExperimentalFeatures();

  const clientOptions: LanguageClientOptions = {
    documentSelector: [{ scheme: "file", language: "machina" }],
    outputChannel: output,
    initializationOptions: {
      experimentalFeatures,
    },
    synchronize: {
      configurationSection: "machina.languageServer",
    },
    middleware: {
      async provideHover(document, position, token, next) {
        const debug = isDebugLoggingEnabled();
        if (debug) {
          output?.appendLine(
            `[lsp] hover request ` +
              `uri=${document.uri.toString()} ` +
              `line=${position.line} ` +
              `col=${position.character} ` +
              `docVersion=${document.version}`
          );
        }

        const result = await next(document, position, token);

        if (debug) {
          output?.appendLine(`[lsp] hover response ${summarizeHoverResult(result)}`);
        }
        return result;
      },
      async provideDefinition(document, position, token, next) {
        const debug = isDebugLoggingEnabled();
        if (debug) {
          output?.appendLine(
            `[lsp] definition request ` +
              `uri=${document.uri.toString()} ` +
              `line=${position.line} ` +
              `col=${position.character} ` +
              `docVersion=${document.version}`
          );
        }

        const result = await next(document, position, token);

        if (debug) {
          output?.appendLine(`[lsp] definition response ${summarizeDefinitionResult(result)}`);
        }
        return result;
      },
      async provideCompletionItem(document, position, context, token, next) {
        const debug = isDebugLoggingEnabled();
        if (debug) {
          output?.appendLine(
            `[lsp] completion request uri=${document.uri.toString()} ` +
              `line=${position.line} ` +
              `col=${position.character} ` +
              `triggerKind=${context.triggerKind} ` +
              `triggerChar=${context.triggerCharacter ?? "<none>"} ` +
              `docVersion=${document.version}`
          );
        }

        const result = await next(document, position, context, token);

        if (debug) {
          output?.appendLine(
            `[lsp] completion response ${summarizeCompletionResult(result)}`
          );
          const labels = completionLabels(result).slice(0, 25);
          output?.appendLine(
            `[lsp] completion labels(${labels.length}): ${labels.join(", ")}`
          );
        }
        return result;
      },
      async provideCodeActions(document, range, context, token, next) {
        const debug = isDebugLoggingEnabled();
        if (debug) {
          output?.appendLine(
            `[lsp] codeAction request ` +
              `uri=${document.uri.toString()} ` +
              `range=` +
                `(${range.start.line}:${range.start.character})-` +
                `(${range.end.line}:${range.end.character}) ` +
              `contextDiagnostics=${context.diagnostics.length} ` +
              `docVersion=${document.version}`
          );
        }

        const result = await next(document, range, context, token);

        if (debug) {
          output?.appendLine(`[lsp] codeAction response ${summarizeCodeActionResult(result)}`);
        }
        return result;
      },
      async provideSignatureHelp(document, position, context, token, next) {
        const debug = isDebugLoggingEnabled();
        if (debug) {
          output?.appendLine(
            `[lsp] signatureHelp request ` +
              `uri=${document.uri.toString()} ` +
              `line=${position.line} ` +
              `col=${position.character} ` +
              `triggerKind=${context.triggerKind} ` +
              `triggerChar=${context.triggerCharacter ?? "<none>"} ` +
              `isRetrigger=${context.isRetrigger} ` +
              `docVersion=${document.version}`
          );
        }

        const result = await next(document, position, context, token);

        if (debug) {
          output?.appendLine(`[lsp] signatureHelp response ${summarizeSignatureHelpResult(result)}`);
        }
        return result;
      },
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

function configuredExperimentalFeatures(): string[] {
  const cfg = vscode.workspace.getConfiguration("machina.languageServer");
  const raw = cfg.get<unknown>("experimentalFeatures", []);
  if (!Array.isArray(raw)) {
    return [];
  }
  return raw
    .filter((value): value is string => typeof value === "string")
    .map((feature) => feature.trim())
    .filter((feature) => feature.length > 0);
}

function formatError(error: unknown): string {
  if (error instanceof Error) {
    return error.stack ?? error.message;
  }
  return String(error);
}

function isDebugLoggingEnabled(): boolean {
  return vscode.workspace
    .getConfiguration("machina.languageServer")
    .get<boolean>("debugLogging", false);
}

function summarizeDefinitionResult(
  result:
    | vscode.Definition
    | vscode.LocationLink[]
    | null
    | undefined
): string {
  if (result == null) {
    return "result=null";
  }
  if (Array.isArray(result)) {
    return `result=array(len=${result.length})`;
  }
  return "result=single-location";
}

function summarizeHoverResult(result: vscode.Hover | null | undefined): string {
  if (result == null) {
    return "result=null";
  }
  return "result=hover";
}

function summarizeCompletionResult(
  result:
    | vscode.CompletionItem[]
    | vscode.CompletionList<vscode.CompletionItem>
    | null
    | undefined
): string {
  if (result == null) {
    return "result=null";
  }
  if (Array.isArray(result)) {
    return `result=array(len=${result.length})`;
  }
  return `result=list(` +
    `len=${result.items.length}, ` +
    `isIncomplete=${result.isIncomplete})`;
}

function completionLabels(
  result:
    | vscode.CompletionItem[]
    | vscode.CompletionList<vscode.CompletionItem>
    | null
    | undefined
): string[] {
  if (result == null) {
    return [];
  }
  const items = Array.isArray(result) ? result : result.items;
  return items.map((item) =>
    typeof item.label === "string" ? item.label : item.label.label
  );
}

function summarizeCodeActionResult(
  result:
    | readonly (vscode.Command | vscode.CodeAction)[]
    | vscode.Command
    | null
    | undefined
): string {
  if (result == null) {
    return "result=null";
  }
  if (Array.isArray(result)) {
    return `result=array(len=${result.length})`;
  }
  return "result=single-command";
}

function summarizeSignatureHelpResult(
  result: vscode.SignatureHelp | null | undefined
): string {
  if (result == null) {
    return "result=null";
  }
  const sigCount = result.signatures.length;
  const activeSig = result.activeSignature ?? 0;
  const activeParam = result.activeParameter ?? 0;
  return `result=signatureHelp(` +
    `signatures=${sigCount}, ` +
    `activeSignature=${activeSig}, ` +
    `activeParameter=${activeParam})`;
}
