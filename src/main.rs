use clap::Parser as ClapParser;
use machina::compile::{CompileOptions, compile_with_path};
use machina::diag::{CompileError, Span, format_error};
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
                    CompileError::Frontend(e) => {
                        println!("{e}");
                    }
                    CompileError::Io(path, e) => {
                        println!("{}: {}", path.display(), e);
                    }
                }
            }
        }
    }
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
