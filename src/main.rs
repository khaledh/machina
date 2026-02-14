use clap::Parser as ClapParser;
use machina::core::capsule::CapsuleError;
use machina::core::diag::{CompileError, Position, Span, format_error};
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use machina::services::analysis::db::AnalysisDb;
use machina::services::analysis::diagnostics::Diagnostic;
use machina::services::analysis::diagnostics::DiagnosticPhase;
use std::ffi::OsStr;
use std::path::{Path, PathBuf};
use std::process::Command as ProcessCommand;

#[derive(ClapParser)]
#[command(
    name = "Machina Compiler",
    version,
    long_version = concat!(
        env!("CARGO_PKG_VERSION"), "\n",
        "Copyright (c) 2025 ", env!("CARGO_PKG_AUTHORS")
    ),
    about,
    long_about = None
)]
struct Args {
    #[clap(subcommand)]
    cmd: Command,

    /// Comma-separated list of compiler debug dumps: [tokens, ast, deftab,
    /// typemap, nrvo, ir, asm]
    #[clap(long, global = true)]
    dump: Option<String>,

    /// Target architecture (arm64 only for now)
    #[clap(long, value_enum, default_value_t = TargetKind::Arm64, global = true)]
    target: TargetKind,

    /// Comma-separated list of artifacts to emit: [asm, ir]
    #[clap(long, value_delimiter = ',', global = true)]
    emit: Vec<EmitKind>,

    /// Emit allocation trace messages from the runtime.
    #[clap(long = "trace-alloc", global = true)]
    trace_alloc: bool,

    /// Emit SSA drop trace comments into the IR.
    #[clap(long = "trace-drops", global = true)]
    trace_drops: bool,

    /// Verify SSA IR invariants after SSA lowering/optimization.
    #[clap(long = "verify-ir", global = true)]
    verify_ir: bool,

    /// Enable experimental language features (comma-separated).
    #[clap(long, value_delimiter = ',', global = true)]
    experimental: Vec<ExperimentalFeature>,
}

#[derive(clap::Subcommand)]
enum Command {
    Compile {
        /// Input source file path
        input: String,

        /// Output object file path
        #[clap(short, long)]
        output: Option<String>,
    },
    Build {
        /// Input source file path
        input: String,

        /// Output executable file path
        #[clap(short, long)]
        output: Option<String>,
    },
    Run {
        /// Input source file path
        input: String,
    },
    Check {
        /// Input source file path
        input: String,
    },
    Query {
        /// Input source file path
        input: String,

        /// Query position formatted as line:col (1-based).
        #[clap(long)]
        pos: String,

        /// Lookup kind to run.
        #[clap(long, value_enum, default_value_t = QueryLookupKind::Hover)]
        kind: QueryLookupKind,

        /// New identifier name for rename planning (required for `--kind rename`).
        #[clap(long)]
        new_name: Option<String>,
    },
}

#[derive(clap::ValueEnum, Clone)]
enum EmitKind {
    Asm,
    Ir,
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum TargetKind {
    Arm64,
}

#[derive(clap::ValueEnum, Clone, Copy)]
enum QueryLookupKind {
    Def,
    Type,
    Hover,
    Completions,
    Signature,
    References,
    Rename,
    DocumentSymbols,
    SemanticTokens,
    CodeActions,
}

#[derive(clap::ValueEnum, Clone, Copy, PartialEq, Eq)]
enum ExperimentalFeature {
    Typestate,
}

#[derive(Copy, Clone)]
enum DriverKind {
    Compile,
    Build,
    Run,
}

struct DriverInvocation {
    input_path: PathBuf,
    output: Option<PathBuf>,
    kind: DriverKind,
}

enum DriverResult {
    Success { remove_asm: bool },
    RunExit { remove_asm: bool, exit_code: i32 },
}

fn main() {
    let Args {
        cmd,
        dump,
        target: _target,
        emit,
        trace_alloc,
        trace_drops,
        verify_ir,
        experimental,
    } = Args::parse();
    let experimental_typestate = experimental.contains(&ExperimentalFeature::Typestate);
    let invocation = match cmd {
        Command::Compile { input, output } => DriverInvocation {
            input_path: PathBuf::from(input),
            output: output.map(PathBuf::from),
            kind: DriverKind::Compile,
        },
        Command::Build { input, output } => DriverInvocation {
            input_path: PathBuf::from(input),
            output: output.map(PathBuf::from),
            kind: DriverKind::Build,
        },
        Command::Run { input } => DriverInvocation {
            input_path: PathBuf::from(input),
            output: None,
            kind: DriverKind::Run,
        },
        Command::Check { input } => {
            let input_path = PathBuf::from(input);
            match run_check(&input_path, experimental_typestate) {
                Ok(0) => {}
                Ok(_) => std::process::exit(1),
                Err(message) => {
                    println!("[ERROR] check failed: {message}");
                    std::process::exit(1);
                }
            }
            return;
        }
        Command::Query {
            input,
            pos,
            kind,
            new_name,
        } => {
            let input_path = PathBuf::from(input);
            match run_query(&input_path, &pos, kind, new_name.as_deref()) {
                Ok(0) => {}
                Ok(_) => std::process::exit(1),
                Err(message) => {
                    println!("[ERROR] query failed: {message}");
                    std::process::exit(1);
                }
            }
            return;
        }
    };
    let input_path = invocation.input_path.as_path();
    let source = match std::fs::read_to_string(input_path) {
        Ok(source) => source,
        Err(e) => {
            println!("[ERROR] failed to read {}: {e}", input_path.display());
            return;
        }
    };
    let emit_asm = emit.iter().any(|kind| matches!(kind, EmitKind::Asm));
    let emit_ir = emit.iter().any(|kind| matches!(kind, EmitKind::Ir));
    let opts = CompileOptions {
        dump,
        emit_ir,
        verify_ir,
        trace_alloc,
        trace_drops,
        inject_prelude: true,
        experimental_typestate,
    };
    let output = compile_with_path(&source, Some(input_path), &opts);

    match output {
        Ok(output) => {
            if let Some(ir) = output.ir {
                let ir_path = input_path.with_extension("ir");
                if let Err(e) = std::fs::write(&ir_path, ir) {
                    eprintln!("[WARN] failed to write {}: {e}", ir_path.display());
                }
            }

            let asm_path = if emit_asm {
                input_path.with_extension("s")
            } else {
                temp_asm_path(input_path)
            };
            if let Err(e) = std::fs::write(&asm_path, output.asm) {
                println!("[ERROR] failed to write {}: {e}", asm_path.display());
                return;
            }

            let result = match invocation.kind {
                DriverKind::Compile => {
                    let obj_path = invocation
                        .output
                        .clone()
                        .unwrap_or_else(|| input_path.with_extension("o"));
                    let result = assemble_object(&asm_path, &obj_path);
                    if result.is_ok() {
                        println!("[SUCCESS] object written to {}", obj_path.display());
                    }
                    let remove_asm = result.is_ok();
                    result.map(|_| DriverResult::Success { remove_asm })
                }
                DriverKind::Build => {
                    let exe_path = invocation
                        .output
                        .clone()
                        .unwrap_or_else(|| default_exe_path(input_path));
                    let result = compile_prelude_impl_object(
                        &opts,
                        input_path.parent().unwrap_or_else(|| Path::new(".")),
                    )
                    .and_then(|(asm, obj)| {
                        let link_result =
                            link_executable(&asm_path, std::slice::from_ref(&obj), &exe_path);
                        if link_result.is_ok() {
                            let _ = std::fs::remove_file(&asm);
                            let _ = std::fs::remove_file(&obj);
                        }
                        link_result
                    });
                    if result.is_ok() {
                        println!("[SUCCESS] executable written to {}", exe_path.display());
                    }
                    let remove_asm = result.is_ok();
                    result.map(|_| DriverResult::Success { remove_asm })
                }
                DriverKind::Run => {
                    let exe_path = default_exe_path(input_path);
                    let link_result = compile_prelude_impl_object(
                        &opts,
                        input_path.parent().unwrap_or_else(|| Path::new(".")),
                    )
                    .and_then(|(asm, obj)| {
                        let link_result =
                            link_executable(&asm_path, std::slice::from_ref(&obj), &exe_path);
                        if link_result.is_ok() {
                            let _ = std::fs::remove_file(&asm);
                            let _ = std::fs::remove_file(&obj);
                        }
                        link_result
                    });
                    let remove_asm = link_result.is_ok();
                    let result = link_result.and_then(|_| run_executable(&exe_path));
                    result.map(|exit_code| DriverResult::RunExit {
                        remove_asm,
                        exit_code,
                    })
                }
            };

            if let Err(message) = result {
                let command = match invocation.kind {
                    DriverKind::Compile => "compile",
                    DriverKind::Build => "build",
                    DriverKind::Run => "run",
                };
                println!("[ERROR] {command} failed: {message}");
            } else if let Ok(driver_result) = result {
                let (remove_asm, exit_code) = match driver_result {
                    DriverResult::Success { remove_asm } => (remove_asm, None),
                    DriverResult::RunExit {
                        remove_asm,
                        exit_code,
                    } => (remove_asm, Some(exit_code)),
                };
                if remove_asm && !emit_asm {
                    let _ = std::fs::remove_file(&asm_path);
                }
                if let Some(exit_code) = exit_code {
                    std::process::exit(exit_code);
                }
            }
        }
        Err(errors) => {
            for error in errors {
                match error {
                    CompileError::Lex(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::Parse(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::Resolve(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::TypeCheck(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::SemCheck(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::Monomorphize(e) => {
                        println!("{}", format_error(&source, e.span(), e));
                    }
                    CompileError::LowerToIr(e) => {
                        println!("{}", format_error(&source, Span::default(), e));
                    }
                    CompileError::VerifyIr(e) => {
                        println!("{}", format_error(&source, Span::default(), e));
                    }
                    CompileError::Capsule(e) => {
                        println!("{e}");
                    }
                    CompileError::Io(path, e) => {
                        println!("{}: {}", path.display(), e);
                    }
                    CompileError::QueryCancelled => {
                        println!("analysis query cancelled");
                    }
                }
            }
        }
    }
}

fn run_check(input_path: &Path, experimental_typestate: bool) -> Result<usize, String> {
    let source = std::fs::read_to_string(input_path)
        .map_err(|e| format!("failed to read {}: {e}", input_path.display()))?;
    match check_with_path(&source, input_path, true, experimental_typestate) {
        Ok(()) => {
            println!("[OK] no diagnostics");
            Ok(0)
        }
        Err(errors) => {
            let mut count = 0usize;
            for error in errors {
                count += print_check_error(&source, error);
            }
            Ok(count)
        }
    }
}

fn print_check_error(entry_source: &str, error: CompileError) -> usize {
    match error {
        CompileError::Lex(e) => {
            print_structured_diag(entry_source, Diagnostic::from_lex_error(&e));
            1
        }
        CompileError::Parse(e) => {
            print_structured_diag(entry_source, Diagnostic::from_parse_error(&e));
            1
        }
        CompileError::Resolve(e) => {
            print_structured_diag(entry_source, Diagnostic::from_resolve_error(&e));
            1
        }
        CompileError::TypeCheck(e) => {
            print_structured_diag(entry_source, Diagnostic::from_typecheck_error(&e));
            1
        }
        CompileError::Capsule(capsule_error) => print_capsule_check_error(capsule_error),
        CompileError::Io(path, err) => {
            println!("{}: {}", path.display(), err);
            1
        }
        CompileError::QueryCancelled => {
            println!("analysis query cancelled");
            1
        }
        other => {
            println!("{other}");
            1
        }
    }
}

fn print_capsule_check_error(error: CapsuleError) -> usize {
    match error {
        CapsuleError::Lex { path, error } => {
            let source = std::fs::read_to_string(&path).unwrap_or_default();
            print_structured_diag(&source, Diagnostic::from_lex_error(&error));
            1
        }
        CapsuleError::Parse { path, error } => {
            let source = std::fs::read_to_string(&path).unwrap_or_default();
            print_structured_diag(&source, Diagnostic::from_parse_error(&error));
            1
        }
        other => {
            println!("{other}");
            1
        }
    }
}

fn print_structured_diag(source: &str, diag: Diagnostic) {
    let phase = match diag.phase {
        DiagnosticPhase::Parse => "parse",
        DiagnosticPhase::Resolve => "resolve",
        DiagnosticPhase::Typecheck => "typecheck",
        DiagnosticPhase::Semcheck => "semcheck",
    };
    let message = format!("[{phase}:{}] {}", diag.code, diag.message);
    println!("{}", format_error(source, diag.span, message));
}

fn run_query(
    input_path: &Path,
    pos: &str,
    kind: QueryLookupKind,
    new_name: Option<&str>,
) -> Result<usize, String> {
    let source = std::fs::read_to_string(input_path)
        .map_err(|e| format!("failed to read {}: {e}", input_path.display()))?;
    let (line, col) = parse_pos_arg(pos)?;
    let position = position_from_line_col(&source, line, col)?;
    let query_span = Span {
        start: position,
        end: position,
    };

    let mut db = AnalysisDb::new();
    let file_id = db.upsert_disk_text(input_path.to_path_buf(), source.as_str());

    match kind {
        QueryLookupKind::Def => {
            let def_id = db
                .def_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if let Some(def_id) = def_id {
                println!("def {}", def_id.0);
                Ok(0)
            } else {
                println!("[NONE] no definition at {line}:{col}");
                Ok(1)
            }
        }
        QueryLookupKind::Type => {
            let ty = db
                .type_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if let Some(ty) = ty {
                println!("{ty}");
                Ok(0)
            } else {
                println!("[NONE] no type at {line}:{col}");
                Ok(1)
            }
        }
        QueryLookupKind::Hover => {
            let hover = db
                .hover_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if let Some(hover) = hover {
                println!("{}", hover.display);
                Ok(0)
            } else {
                println!("[NONE] no hover info at {line}:{col}");
                Ok(1)
            }
        }
        QueryLookupKind::Completions => {
            let items = db
                .completions_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if items.is_empty() {
                println!("[NONE] no completions at {line}:{col}");
                return Ok(1);
            }
            for item in items {
                if let Some(detail) = item.detail {
                    println!("{} ({detail})", item.label);
                } else {
                    println!("{}", item.label);
                }
            }
            Ok(0)
        }
        QueryLookupKind::Signature => {
            let sig = db
                .signature_help_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if let Some(sig) = sig {
                println!("{}", sig.label);
                println!("active_parameter={}", sig.active_parameter);
                Ok(0)
            } else {
                println!("[NONE] no signature info at {line}:{col}");
                Ok(1)
            }
        }
        QueryLookupKind::References => {
            let def_id = db
                .def_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            let Some(def_id) = def_id else {
                println!("[NONE] no definition at {line}:{col}");
                return Ok(1);
            };

            let refs = db
                .references(def_id)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if refs.is_empty() {
                println!("[NONE] no references for def {}", def_id.0);
                return Ok(1);
            }
            println!("def {} references {}", def_id.0, refs.len());
            for loc in refs {
                println!(
                    "{}",
                    format_location(&loc.path, loc.file_id.0 as u64, loc.span)
                );
            }
            Ok(0)
        }
        QueryLookupKind::Rename => {
            let Some(new_name) = new_name else {
                return Err("`--new-name` is required for `--kind rename`".to_string());
            };
            let def_id = db
                .def_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            let Some(def_id) = def_id else {
                println!("[NONE] no definition at {line}:{col}");
                return Ok(1);
            };

            let plan = db
                .rename_plan(def_id, new_name)
                .map_err(|_| "analysis query cancelled".to_string())?;
            let old_name = plan.old_name.as_deref().unwrap_or("<unknown>");
            let can_apply = plan.can_apply();
            println!(
                "rename {} {} -> {} (edits: {}, conflicts: {}, can_apply: {})",
                plan.def_id.0,
                old_name,
                plan.new_name,
                plan.edits.len(),
                plan.conflicts.len(),
                can_apply
            );
            for conflict in &plan.conflicts {
                match conflict.existing_def {
                    Some(existing) => {
                        println!("conflict [{}]: {}", existing.0, conflict.message);
                    }
                    None => println!("conflict: {}", conflict.message),
                }
            }
            for edit in &plan.edits {
                println!(
                    "edit {} => `{}`",
                    format_location(
                        &edit.location.path,
                        edit.location.file_id.0 as u64,
                        edit.location.span
                    ),
                    edit.replacement
                );
            }
            if can_apply { Ok(0) } else { Ok(1) }
        }
        QueryLookupKind::DocumentSymbols => {
            let symbols = db
                .document_symbols_at_file(file_id)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if symbols.is_empty() {
                println!("[NONE] no document symbols");
                return Ok(1);
            }
            for sym in symbols {
                println!(
                    "{} [{}] {}",
                    sym.name,
                    sym.def_id.0,
                    format_location(&Some(input_path.to_path_buf()), file_id.0 as u64, sym.span)
                );
            }
            Ok(0)
        }
        QueryLookupKind::SemanticTokens => {
            let tokens = db
                .semantic_tokens_at_file(file_id)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if tokens.is_empty() {
                println!("[NONE] no semantic tokens");
                return Ok(1);
            }
            for token in tokens {
                println!(
                    "{:?} [{}] {}",
                    token.kind,
                    token.def_id.0,
                    format_location(
                        &Some(input_path.to_path_buf()),
                        file_id.0 as u64,
                        token.span
                    )
                );
            }
            Ok(0)
        }
        QueryLookupKind::CodeActions => {
            let actions = db
                .code_actions_at_file(file_id, query_span)
                .map_err(|_| "analysis query cancelled".to_string())?;
            if actions.is_empty() {
                println!("[NONE] no code actions at {line}:{col}");
                return Ok(1);
            }
            for action in actions {
                println!("{} [{}]", action.title, action.diagnostic_code);
                for edit in action.edits {
                    println!(
                        "  edit {} => `{}`",
                        format_location(
                            &Some(input_path.to_path_buf()),
                            file_id.0 as u64,
                            edit.span
                        ),
                        edit.new_text
                    );
                }
            }
            Ok(0)
        }
    }
}

fn format_location(path: &Option<PathBuf>, file_id: u64, span: Span) -> String {
    let base = path
        .as_ref()
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| format!("<file:{file_id}>"));
    format!(
        "{}:{}:{}-{}:{}",
        base, span.start.line, span.start.column, span.end.line, span.end.column
    )
}

fn parse_pos_arg(pos: &str) -> Result<(usize, usize), String> {
    let (line, col) = pos
        .split_once(':')
        .ok_or_else(|| format!("invalid --pos format `{pos}`; expected line:col"))?;
    let line = line
        .parse::<usize>()
        .map_err(|_| format!("invalid line in --pos `{pos}`"))?;
    let col = col
        .parse::<usize>()
        .map_err(|_| format!("invalid column in --pos `{pos}`"))?;
    if line == 0 || col == 0 {
        return Err(format!("--pos must be 1-based, got `{pos}`"));
    }
    Ok((line, col))
}

fn position_from_line_col(source: &str, line: usize, col: usize) -> Result<Position, String> {
    let mut curr_line = 1usize;
    let mut curr_col = 1usize;
    for (offset, ch) in source.char_indices() {
        if curr_line == line && curr_col == col {
            return Ok(Position {
                offset,
                line,
                column: col,
            });
        }
        if ch == '\n' {
            curr_line += 1;
            curr_col = 1;
        } else {
            curr_col += 1;
        }
    }
    if curr_line == line && curr_col == col {
        return Ok(Position {
            offset: source.len(),
            line,
            column: col,
        });
    }
    Err(format!(
        "position {line}:{col} is outside source range (line={curr_line}, col={curr_col})"
    ))
}

fn default_exe_path(input_path: &Path) -> PathBuf {
    let stem = input_path.file_stem().unwrap_or_else(|| OsStr::new("out"));
    let mut path = input_path.to_path_buf();
    path.set_file_name(stem);
    path
}

fn temp_asm_path(input_path: &Path) -> PathBuf {
    let stem = input_path
        .file_stem()
        .unwrap_or_else(|| OsStr::new("out"))
        .to_string_lossy();
    let pid = std::process::id();
    let mut path = std::env::temp_dir();
    path.push(format!("machina_{pid}_{stem}.s"));
    path
}

fn assemble_object(asm_path: &Path, obj_path: &Path) -> Result<(), String> {
    let status = ProcessCommand::new("cc")
        .arg("-c")
        .arg("-o")
        .arg(obj_path)
        .arg(asm_path)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
}

fn link_executable(asm_path: &Path, extra_objs: &[PathBuf], exe_path: &Path) -> Result<(), String> {
    let runtime_paths = runtime_source_paths();
    for runtime_path in &runtime_paths {
        if !runtime_path.exists() {
            return Err(format!(
                "runtime source file not found at {}",
                runtime_path.display()
            ));
        }
    }
    let status = ProcessCommand::new("cc")
        .arg("-o")
        .arg(exe_path)
        .arg(asm_path)
        .args(extra_objs)
        .args(&runtime_paths)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
}

fn temp_obj_path(name: &str) -> PathBuf {
    let pid = std::process::id();
    let mut path = std::env::temp_dir();
    path.push(format!("machina_{pid}_{name}.o"));
    path
}

fn temp_named_asm_path(name: &str) -> PathBuf {
    let pid = std::process::id();
    let mut path = std::env::temp_dir();
    path.push(format!("machina_{pid}_{name}.s"));
    path
}

fn compile_prelude_impl_object(
    opts: &CompileOptions,
    ir_dir: &Path,
) -> Result<(PathBuf, PathBuf), String> {
    let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_impl.mc");
    let prelude_src = std::fs::read_to_string(&prelude_path)
        .map_err(|e| format!("failed to read {}: {e}", prelude_path.display()))?;

    let impl_opts = CompileOptions {
        dump: None,
        emit_ir: opts.emit_ir,
        verify_ir: opts.verify_ir,
        trace_alloc: opts.trace_alloc,
        trace_drops: opts.trace_drops,
        inject_prelude: true,
        experimental_typestate: opts.experimental_typestate,
    };

    let output =
        compile_with_path(&prelude_src, Some(&prelude_path), &impl_opts).map_err(|errs| {
            let mut message = String::new();
            for err in errs {
                message.push_str(&format!("{err}\n"));
            }
            message
        })?;

    if let Some(ir) = output.ir.as_ref() {
        let ir_path = ir_dir.join("prelude_impl.ir");
        if let Err(e) = std::fs::write(&ir_path, ir) {
            eprintln!("[WARN] failed to write {}: {e}", ir_path.display());
        }
    }

    let asm_path = temp_named_asm_path("prelude_impl");
    let obj_path = temp_obj_path("prelude_impl");

    std::fs::write(&asm_path, output.asm)
        .map_err(|e| format!("failed to write {}: {e}", asm_path.display()))?;
    assemble_object(&asm_path, &obj_path)?;

    Ok((asm_path, obj_path))
}

fn run_executable(exe_path: &Path) -> Result<i32, String> {
    let run_path = if exe_path.components().count() == 1 {
        PathBuf::from(format!("./{}", exe_path.display()))
    } else {
        exe_path.to_path_buf()
    };
    let status = ProcessCommand::new(&run_path)
        .status()
        .map_err(|e| format!("failed to run {}: {e}", run_path.display()))?;
    Ok(status.code().unwrap_or(1))
}

const RUNTIME_SOURCE_FILES: &[&str] = &[
    "alloc.c",
    "conv.c",
    "dyn_array.c",
    "hash_table.c",
    "map_table.c",
    "mem.c",
    "print.c",
    "set.c",
    "string.c",
    "trap.c",
];

fn runtime_source_paths() -> Vec<PathBuf> {
    let runtime_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("runtime");
    RUNTIME_SOURCE_FILES
        .iter()
        .map(|f| runtime_dir.join(f))
        .collect()
}
