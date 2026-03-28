use clap::Parser as ClapParser;
use machina::core::capsule::CapsuleError;
use machina::core::diag::{CompileError, Span, format_error};
use machina::driver::compile::{CompileOptions, check_with_path, compile_with_path};
use machina::driver::native_support::{assemble_object, default_exe_path, ensure_runtime_archive};
use machina::driver::query::{QueryLookupKind as DriverQueryLookupKind, run_query};
use machina::services::analysis::diagnostics::Diagnostic;
use machina::services::analysis::diagnostics::DiagnosticPhase;
use std::ffi::OsStr;
use std::fs;
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

impl From<QueryLookupKind> for DriverQueryLookupKind {
    fn from(value: QueryLookupKind) -> Self {
        match value {
            QueryLookupKind::Def => DriverQueryLookupKind::Def,
            QueryLookupKind::Type => DriverQueryLookupKind::Type,
            QueryLookupKind::Hover => DriverQueryLookupKind::Hover,
            QueryLookupKind::Completions => DriverQueryLookupKind::Completions,
            QueryLookupKind::Signature => DriverQueryLookupKind::Signature,
            QueryLookupKind::References => DriverQueryLookupKind::References,
            QueryLookupKind::Rename => DriverQueryLookupKind::Rename,
            QueryLookupKind::DocumentSymbols => DriverQueryLookupKind::DocumentSymbols,
            QueryLookupKind::SemanticTokens => DriverQueryLookupKind::SemanticTokens,
            QueryLookupKind::CodeActions => DriverQueryLookupKind::CodeActions,
        }
    }
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
    } = Args::parse();
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
            match run_check(&input_path) {
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
            match run_query(&input_path, &pos, kind.into(), new_name.as_deref()) {
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
    let source = match fs::read_to_string(input_path) {
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
        use_stdlib_objects: true,
    };
    let output = compile_with_path(&source, Some(input_path), &opts);

    match output {
        Ok(output) => {
            if let Some(ir) = output.ir {
                let ir_path = input_path.with_extension("ir");
                if let Err(e) = fs::write(&ir_path, ir) {
                    eprintln!("[WARN] failed to write {}: {e}", ir_path.display());
                }
            }

            let asm_path = if emit_asm {
                input_path.with_extension("s")
            } else {
                temp_asm_path(input_path)
            };
            if let Err(e) = fs::write(&asm_path, output.asm) {
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
                    let result = link_executable(&asm_path, &output.extra_link_paths, &exe_path);
                    if result.is_ok() {
                        println!("[SUCCESS] executable written to {}", exe_path.display());
                    }
                    let remove_asm = result.is_ok();
                    result.map(|_| DriverResult::Success { remove_asm })
                }
                DriverKind::Run => {
                    let exe_path = default_exe_path(input_path);
                    let link_result =
                        link_executable(&asm_path, &output.extra_link_paths, &exe_path);
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

fn run_check(input_path: &Path) -> Result<usize, String> {
    let source = fs::read_to_string(input_path)
        .map_err(|e| format!("failed to read {}: {e}", input_path.display()))?;
    match check_with_path(&source, input_path, true) {
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
            let source = fs::read_to_string(&path).unwrap_or_default();
            print_structured_diag(&source, Diagnostic::from_lex_error(&error));
            1
        }
        CapsuleError::Parse { path, error } => {
            let source = fs::read_to_string(&path).unwrap_or_default();
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

fn link_executable(asm_path: &Path, extra_objs: &[PathBuf], exe_path: &Path) -> Result<(), String> {
    let runtime_archive = ensure_runtime_archive()?;
    let status = ProcessCommand::new("cc")
        .arg("-o")
        .arg(exe_path)
        .arg(asm_path)
        .args(extra_objs)
        .arg(runtime_archive)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
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
