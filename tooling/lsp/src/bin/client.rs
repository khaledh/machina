use std::io::{self, BufRead, Write};
use std::path::PathBuf;

use clap::Parser as ClapParser;
use machina::services::analysis::trace::{AnalysisTraceCategory, AnalysisTracer};
use machina_lsp::client::{ClientError, LspClient};
use serde_json::Value;

#[derive(ClapParser, Debug)]
#[command(
    name = "machina-lsp-client",
    version,
    about = "Interact with Machina LSP handlers as a client",
    long_about = None
)]
struct Args {
    #[arg(long, value_delimiter = ',', global = true)]
    trace: Vec<String>,

    #[command(subcommand)]
    command: Option<Command>,
}

#[derive(clap::Subcommand, Debug, Clone, PartialEq, Eq)]
enum Command {
    Diagnostics { file: PathBuf },
    Hover { file: PathBuf, pos: String },
    Definition { file: PathBuf, pos: String },
    Completion { file: PathBuf, pos: String },
    Signature { file: PathBuf, pos: String },
}

fn main() {
    let args = Args::parse();
    let trace_categories = parse_trace_categories(&args.trace).unwrap_or_else(|error| {
        eprintln!("machina-lsp-client: {error}");
        std::process::exit(1);
    });
    if let Some(command) = args.command {
        if let Err(error) = run_one_shot(command, &trace_categories) {
            eprintln!("machina-lsp-client: {error}");
            std::process::exit(1);
        }
    } else if let Err(error) = run_repl(trace_categories) {
        eprintln!("machina-lsp-client: {error}");
        std::process::exit(1);
    }
}

fn run_one_shot(
    command: Command,
    trace_categories: &[AnalysisTraceCategory],
) -> Result<(), ClientError> {
    let mut client = LspClient::new();
    client.set_tracer(tracer_for_categories(trace_categories));
    let mut stdout = io::stdout();
    match command {
        Command::Diagnostics { file } => {
            let response = client.diagnostics(&file)?;
            print_diagnostics_response(&mut stdout, &response)
                .expect("stdout write should succeed");
        }
        Command::Hover { file, pos } => {
            let (line, col) = parse_position(&pos)?;
            let response = client.hover(&file, line, col)?;
            print_hover_response(&mut stdout, &response).expect("stdout write should succeed");
        }
        Command::Definition { file, pos } => {
            let (line, col) = parse_position(&pos)?;
            let response = client.definition(&file, line, col)?;
            print_definition_response(&mut stdout, &response).expect("stdout write should succeed");
        }
        Command::Completion { file, pos } => {
            let (line, col) = parse_position(&pos)?;
            let response = client.completion(&file, line, col)?;
            print_completion_response(&mut stdout, &response).expect("stdout write should succeed");
        }
        Command::Signature { file, pos } => {
            let (line, col) = parse_position(&pos)?;
            let response = client.signature(&file, line, col)?;
            print_signature_response(&mut stdout, &response).expect("stdout write should succeed");
        }
    }
    Ok(())
}

fn run_repl(trace_categories: Vec<AnalysisTraceCategory>) -> io::Result<()> {
    let stdin = io::stdin();
    let mut stdout = io::stdout();
    run_repl_with_io(stdin.lock(), &mut stdout, trace_categories)
}

fn print_repl_help(stdout: &mut impl Write) -> io::Result<()> {
    writeln!(stdout, "Commands:")?;
    writeln!(stdout, "  open <file>")?;
    writeln!(stdout, "  diagnostics")?;
    writeln!(stdout, "  hover <line:col>")?;
    writeln!(stdout, "  definition <line:col>")?;
    writeln!(stdout, "  completion <line:col>")?;
    writeln!(stdout, "  signature <line:col>")?;
    writeln!(stdout, "  change <file>")?;
    writeln!(stdout, "  close")?;
    writeln!(stdout, "  trace <cats|off>")?;
    writeln!(stdout, "  quit | exit")?;
    Ok(())
}

fn run_repl_with_io(
    mut input: impl BufRead,
    stdout: &mut impl Write,
    trace_categories: Vec<AnalysisTraceCategory>,
) -> io::Result<()> {
    let mut client = LspClient::new();
    client.set_tracer(tracer_for_categories(&trace_categories));
    let mut state = ReplState {
        trace_categories,
        ..ReplState::default()
    };
    writeln!(
        stdout,
        "Machina LSP client REPL. Type `help` for commands, `exit` to quit."
    )?;
    stdout.flush()?;

    let mut line = String::new();
    loop {
        line.clear();
        if input.read_line(&mut line)? == 0 {
            return Ok(());
        }
        let command = line.trim();
        if command.is_empty() {
            continue;
        }
        if !execute_repl_command(&mut client, &mut state, command, stdout)? {
            return Ok(());
        }
        stdout.flush()?;
    }
}

#[derive(Default)]
struct ReplState {
    current_file: Option<PathBuf>,
    trace_categories: Vec<AnalysisTraceCategory>,
}

fn execute_repl_command(
    client: &mut LspClient,
    state: &mut ReplState,
    command: &str,
    stdout: &mut impl Write,
) -> io::Result<bool> {
    let parts: Vec<&str> = command.split_whitespace().collect();
    let Some(cmd) = parts.first().copied() else {
        return Ok(true);
    };

    match cmd {
        "exit" | "quit" => Ok(false),
        "help" => {
            print_repl_help(stdout)?;
            Ok(true)
        }
        "open" => {
            let Some(file) = parts.get(1) else {
                writeln!(stdout, "usage: open <file>")?;
                return Ok(true);
            };
            let path = PathBuf::from(file);
            match client.open_path(&path) {
                Ok(response) => {
                    state.current_file = Some(path.clone());
                    writeln!(stdout, "Opened {}.", path.display())?;
                    print_diagnostics_response(stdout, &response)?;
                }
                Err(error) => writeln!(stdout, "Error: {error}")?,
            }
            Ok(true)
        }
        "change" => {
            let path = match parts.get(1) {
                Some(file) => PathBuf::from(file),
                None => match state.current_file.clone() {
                    Some(path) => path,
                    None => {
                        writeln!(stdout, "No file is open. Use `open <file>`.")?;
                        return Ok(true);
                    }
                },
            };
            match client.change_path(&path) {
                Ok(response) => {
                    state.current_file = Some(path.clone());
                    writeln!(stdout, "Reloaded {}.", path.display())?;
                    print_diagnostics_response(stdout, &response)?;
                }
                Err(error) => writeln!(stdout, "Error: {error}")?,
            }
            Ok(true)
        }
        "close" => {
            let Some(path) = state.current_file.take() else {
                writeln!(stdout, "No file is open.")?;
                return Ok(true);
            };
            match client.close_path(&path) {
                Ok(_) => writeln!(stdout, "Closed {}.", path.display())?,
                Err(error) => writeln!(stdout, "Error: {error}")?,
            }
            Ok(true)
        }
        "trace" => {
            let Some(spec) = parts.get(1).copied() else {
                writeln!(
                    stdout,
                    "trace is {}",
                    format_trace_categories(&state.trace_categories)
                )?;
                return Ok(true);
            };
            match parse_trace_categories(&[spec.to_string()]) {
                Ok(categories) => {
                    state.trace_categories = categories;
                    client.set_tracer(tracer_for_categories(&state.trace_categories));
                    writeln!(
                        stdout,
                        "trace set to {}",
                        format_trace_categories(&state.trace_categories)
                    )?;
                }
                Err(error) => writeln!(stdout, "Error: {error}")?,
            }
            Ok(true)
        }
        "diagnostics" => {
            let Some(path) = state.current_file.clone() else {
                writeln!(stdout, "No file is open. Use `open <file>`.")?;
                return Ok(true);
            };
            match client.diagnostics(&path) {
                Ok(response) => print_diagnostics_response(stdout, &response)?,
                Err(error) => writeln!(stdout, "Error: {error}")?,
            }
            Ok(true)
        }
        "hover" => run_repl_read_command(
            client,
            state,
            parts.get(1).copied(),
            stdout,
            |client, path, line, col| client.hover(path, line, col),
            print_hover_response,
        ),
        "definition" => run_repl_read_command(
            client,
            state,
            parts.get(1).copied(),
            stdout,
            |client, path, line, col| client.definition(path, line, col),
            print_definition_response,
        ),
        "completion" => run_repl_read_command(
            client,
            state,
            parts.get(1).copied(),
            stdout,
            |client, path, line, col| client.completion(path, line, col),
            print_completion_response,
        ),
        "signature" => run_repl_read_command(
            client,
            state,
            parts.get(1).copied(),
            stdout,
            |client, path, line, col| client.signature(path, line, col),
            print_signature_response,
        ),
        _ => {
            writeln!(stdout, "Unknown command: {cmd}")?;
            writeln!(stdout, "Type `help` for commands.")?;
            Ok(true)
        }
    }
}

fn run_repl_read_command<W, Request, Print>(
    client: &mut LspClient,
    state: &ReplState,
    pos: Option<&str>,
    stdout: &mut W,
    request: Request,
    print: Print,
) -> io::Result<bool>
where
    W: Write,
    Request: FnOnce(&mut LspClient, &std::path::Path, usize, usize) -> Result<Value, ClientError>,
    Print: FnOnce(&mut W, &Value) -> io::Result<()>,
{
    let Some(path) = state.current_file.as_ref() else {
        writeln!(stdout, "No file is open. Use `open <file>`.")?;
        return Ok(true);
    };
    let Some(pos) = pos else {
        writeln!(stdout, "position must be formatted as line:col")?;
        return Ok(true);
    };
    let (line, col) = match parse_position(pos) {
        Ok(position) => position,
        Err(error) => {
            writeln!(stdout, "Error: {error}")?;
            return Ok(true);
        }
    };
    match request(client, path, line, col) {
        Ok(response) => print(stdout, &response)?,
        Err(error) => writeln!(stdout, "Error: {error}")?,
    }
    Ok(true)
}

fn parse_position(pos: &str) -> Result<(usize, usize), ClientError> {
    let Some((line, col)) = pos.split_once(':') else {
        return Err(ClientError::InvalidPosition);
    };
    let line = line
        .parse::<usize>()
        .map_err(|_| ClientError::InvalidPosition)?;
    let col = col
        .parse::<usize>()
        .map_err(|_| ClientError::InvalidPosition)?;
    Ok((line, col))
}

fn parse_trace_categories(specs: &[String]) -> Result<Vec<AnalysisTraceCategory>, ClientError> {
    let mut out = Vec::new();
    for spec in specs {
        for part in spec.split(',') {
            let item = part.trim();
            if item.is_empty() {
                continue;
            }
            if item.eq_ignore_ascii_case("off") {
                return Ok(Vec::new());
            }
            let category = match item {
                "query" => AnalysisTraceCategory::Query,
                "pipeline" => AnalysisTraceCategory::Pipeline,
                "program" => AnalysisTraceCategory::Program,
                "hover" => AnalysisTraceCategory::Hover,
                "session" => AnalysisTraceCategory::Session,
                _ => return Err(ClientError::InvalidTraceCategory(item.to_string())),
            };
            if !out.contains(&category) {
                out.push(category);
            }
        }
    }
    Ok(out)
}

fn tracer_for_categories(categories: &[AnalysisTraceCategory]) -> AnalysisTracer {
    if categories.is_empty() {
        AnalysisTracer::off()
    } else {
        AnalysisTracer::stderr(categories.iter().copied())
    }
}

fn format_trace_categories(categories: &[AnalysisTraceCategory]) -> String {
    if categories.is_empty() {
        "off".to_string()
    } else {
        categories
            .iter()
            .map(ToString::to_string)
            .collect::<Vec<_>>()
            .join(",")
    }
}

fn print_diagnostics_response(stdout: &mut impl Write, response: &Value) -> io::Result<()> {
    let diagnostics = response["params"]["diagnostics"]
        .as_array()
        .expect("diagnostics should be an array");
    if diagnostics.is_empty() {
        writeln!(stdout, "No diagnostics.")?;
        return Ok(());
    }
    for diag in diagnostics {
        let line = diag["range"]["start"]["line"].as_u64().unwrap_or(0) + 1;
        let col = diag["range"]["start"]["character"].as_u64().unwrap_or(0) + 1;
        let code = diag["code"].as_str().unwrap_or("unknown");
        let message = diag["message"].as_str().unwrap_or("");
        writeln!(stdout, "{line}:{col} {code} {message}")?;
    }
    Ok(())
}

fn print_hover_response(stdout: &mut impl Write, response: &Value) -> io::Result<()> {
    if response["result"].is_null() {
        writeln!(stdout, "No hover.")?;
        return Ok(());
    }
    let value = response["result"]["contents"]["value"]
        .as_str()
        .unwrap_or("");
    writeln!(stdout, "{value}")?;
    Ok(())
}

fn print_definition_response(stdout: &mut impl Write, response: &Value) -> io::Result<()> {
    let items = response["result"]
        .as_array()
        .expect("definition result should be an array");
    if items.is_empty() {
        writeln!(stdout, "No definition.")?;
        return Ok(());
    }
    for item in items {
        let uri = item["uri"].as_str().unwrap_or("");
        let line = item["range"]["start"]["line"].as_u64().unwrap_or(0) + 1;
        let col = item["range"]["start"]["character"].as_u64().unwrap_or(0) + 1;
        writeln!(stdout, "{uri}:{line}:{col}")?;
    }
    Ok(())
}

fn print_completion_response(stdout: &mut impl Write, response: &Value) -> io::Result<()> {
    let items = response["result"]["items"]
        .as_array()
        .expect("completion result should contain items");
    if items.is_empty() {
        writeln!(stdout, "No completions.")?;
        return Ok(());
    }
    for item in items {
        let label = item["label"].as_str().unwrap_or("");
        let detail = item["detail"].as_str().unwrap_or("");
        if detail.is_empty() {
            writeln!(stdout, "{label}")?;
        } else {
            writeln!(stdout, "{label} - {detail}")?;
        }
    }
    Ok(())
}

fn print_signature_response(stdout: &mut impl Write, response: &Value) -> io::Result<()> {
    if response["result"].is_null() {
        writeln!(stdout, "No signature help.")?;
        return Ok(());
    }
    let signatures = response["result"]["signatures"]
        .as_array()
        .expect("signature result should contain signatures");
    if signatures.is_empty() {
        writeln!(stdout, "No signature help.")?;
        return Ok(());
    }
    let active = response["result"]["activeSignature"].as_u64().unwrap_or(0) as usize;
    let label = signatures
        .get(active)
        .and_then(|sig| sig["label"].as_str())
        .unwrap_or("");
    writeln!(stdout, "{label}")?;
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::{
        Args, Command, format_trace_categories, parse_position, parse_trace_categories,
        run_repl_with_io,
    };
    use clap::Parser as _;
    use std::fs;
    use std::io::Cursor;
    use std::path::PathBuf;
    use std::time::{SystemTime, UNIX_EPOCH};

    fn temp_path(stem: &str) -> PathBuf {
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("system time should be after epoch")
            .as_nanos();
        std::env::temp_dir().join(format!("machina_lsp_client_repl_{stem}_{nanos}.mc"))
    }

    #[test]
    fn parse_no_args_enters_repl_mode() {
        let args = Args::try_parse_from(["machina-lsp-client"]).expect("args should parse");
        assert!(args.command.is_none());
    }

    #[test]
    fn parse_hover_command() {
        let args = Args::try_parse_from([
            "machina-lsp-client",
            "hover",
            "examples/basics/csv_grade_rewrite_small.mc",
            "26:17",
        ])
        .expect("hover args should parse");
        assert_eq!(
            args.command,
            Some(Command::Hover {
                file: PathBuf::from("examples/basics/csv_grade_rewrite_small.mc"),
                pos: "26:17".to_string(),
            })
        );
    }

    #[test]
    fn parse_diagnostics_command() {
        let args = Args::try_parse_from([
            "machina-lsp-client",
            "diagnostics",
            "examples/basics/csv_grade_rewrite_small.mc",
        ])
        .expect("diagnostics args should parse");
        assert_eq!(
            args.command,
            Some(Command::Diagnostics {
                file: PathBuf::from("examples/basics/csv_grade_rewrite_small.mc"),
            })
        );
    }

    #[test]
    fn parse_position_requires_line_col() {
        assert!(parse_position("26").is_err());
        assert_eq!(parse_position("26:17").expect("valid position"), (26, 17));
    }

    #[test]
    fn repl_open_and_diagnostics_use_current_file() {
        let path = temp_path("diag");
        fs::write(&path, "fn main() -> u64 { 0 }\n").expect("temp source should write");
        let input = format!("open {}\ndiagnostics\nexit\n", path.display());
        let mut output = Vec::new();

        run_repl_with_io(Cursor::new(input), &mut output, Vec::new()).expect("repl should succeed");

        let text = String::from_utf8(output).expect("output should be utf8");
        assert!(text.contains("Opened"));
        assert!(text.contains("No diagnostics."));

        let _ = fs::remove_file(path);
    }

    #[test]
    fn parse_trace_categories_supports_off_and_lists() {
        assert_eq!(format_trace_categories(&[]), "off");
        let categories = parse_trace_categories(&["query,hover".to_string()])
            .expect("trace categories should parse");
        assert_eq!(format_trace_categories(&categories), "query,hover");
        let off = parse_trace_categories(&["off".to_string()]).expect("off should parse");
        assert!(off.is_empty());
    }
}
