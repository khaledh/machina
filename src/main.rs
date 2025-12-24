use clap::Parser as ClapParser;

mod ast;
mod context;
mod diagnostics;
mod lexer;
mod mcir;
mod nrvo;
mod opt;
mod parser;
mod regalloc;
mod resolve;
mod symtab;
mod targets;
mod typeck;
mod types;

use crate::context::AstContext;
use crate::diagnostics::{CompileError, Span, format_error};
use crate::lexer::{LexError, Lexer, Token};
use crate::nrvo::NrvoAnalyzer;
use crate::parser::Parser;
use crate::resolve::resolve;
use crate::targets::TargetKind;
use crate::typeck::type_check;
use std::path::Path;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input source file path
    input: String,

    /// Comma-separated list of things to dump: ast,defmap,typemap,ir,liveness,intervals,regalloc,asm
    #[clap(long)]
    dump: Option<String>,

    /// Target architecture (e.g. arm64)
    #[clap(long, value_enum, default_value_t = TargetKind::Arm64)]
    target: TargetKind,
}

fn main() {
    let args = Args::parse();
    let input_path = args.input.clone();
    let source = match std::fs::read_to_string(&input_path) {
        Ok(source) => source,
        Err(e) => {
            println!("[ERROR] failed to read {}: {e}", input_path);
            return;
        }
    };
    let output = compile(&source, args);

    match output {
        Ok(asm) => {
            let output_path = Path::new(&input_path).with_extension("s");
            match std::fs::write(&output_path, asm) {
                Ok(_) => {
                    println!("[SUCCESS] assembly written to {}", output_path.display())
                }
                Err(e) => println!("[ERROR] failed to write {}: {e}", output_path.display()),
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
                    CompileError::Lower(e) => {
                        println!("{}", format_error(&source, Span::default(), e));
                    }
                    CompileError::Codegen(e) => {
                        println!("{}", format_error(&source, Span::default(), e));
                    }
                }
            }
        }
    }
}

fn compile(source: &str, args: Args) -> Result<String, Vec<CompileError>> {
    // Parse dump flags from comma-separated list, e.g. --dump ast,ir,liveness
    let mut dump_tokens = false;
    let mut dump_ast = false;
    let mut dump_def_map = false;
    let mut dump_type_map = false;
    let mut dump_nrvo = false;
    let mut dump_ir = false;
    let mut dump_liveness = false;
    let mut dump_intervals = false;
    let mut dump_regalloc = false;
    let mut dump_asm = false;

    if let Some(dump) = &args.dump {
        for item in dump.split(',').map(|s| s.trim().to_lowercase()) {
            match item.as_str() {
                "tokens" => dump_tokens = true,
                "ast" => dump_ast = true,
                "defmap" => dump_def_map = true,
                "typemap" => dump_type_map = true,
                "nrvo" => dump_nrvo = true,
                "ir" => dump_ir = true,
                "liveness" => dump_liveness = true,
                "intervals" => dump_intervals = true,
                "regalloc" => dump_regalloc = true,
                "asm" => dump_asm = true,
                "" => {}
                _ => {
                    eprintln!("[WARN] unknown dump flag: {item}");
                }
            }
        }
    }

    // --- Lex ---

    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    if dump_tokens {
        println!("Tokens:");
        println!("--------------------------------");
        for (i, token) in tokens.iter().enumerate() {
            println!("{}: {}", i, token);
        }
        println!("--------------------------------");
    }

    // --- Parse ---

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().map_err(|e| vec![e.into()])?;

    if dump_ast {
        println!("AST:");
        println!("--------------------------------");
        println!("{}", module);
        println!("--------------------------------");
    }

    // --- Resolve Defs/Uses ---

    let ast_context = AstContext::new(module);

    let resolved_context = resolve(ast_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    if dump_def_map {
        println!("Def Map:");
        println!("--------------------------------");
        println!("{}", resolved_context.def_map);
        println!("--------------------------------");
    }

    // --- Type Check ---

    let type_checked_context = type_check(resolved_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    if dump_type_map {
        println!("Type Map:");
        println!("--------------------------------");
        println!("{}", type_checked_context.type_map);
        println!("--------------------------------");
    }

    // --- NRVO Analysis ---

    let analyzed_context = NrvoAnalyzer::new(type_checked_context).analyze();

    if dump_nrvo {
        println!("NRVO:");
        println!("--------------------------------");
        for def in analyzed_context.def_map.get_nrvo_eligible_defs() {
            println!("{}", def);
        }
        println!("--------------------------------");
    }

    // --- Lower to MCIR ---
    let lowered_context =
        mcir::lower_ast::lower_ast(analyzed_context).map_err(|e| vec![e.into()])?;
    if dump_ir {
        println!("MCIR:");
        println!("--------------------------------");
        for (i, body) in lowered_context.func_bodies.iter().enumerate() {
            let func_name = lowered_context.symbols.func_name(i).unwrap_or("<unknown>");
            println!("{}", format_mcir_body(body, func_name));
            println!("--------------------------------");
        }
    }

    // --- Optimize MCIR ---
    let optimized_context = opt::optimize(lowered_context);

    // --- Write MCIR Dump ---
    let mcir_path = Path::new(&args.input).with_extension("mcir");
    let mut mcir_out = String::new();
    for (i, body) in optimized_context.func_bodies.iter().enumerate() {
        let func_name = optimized_context
            .symbols
            .func_name(i)
            .unwrap_or("<unknown>");
        mcir_out.push_str(&format!("{}\n", format_mcir_body(body, func_name)));
        mcir_out.push_str("\n");
    }
    if let Err(e) = std::fs::write(&mcir_path, mcir_out) {
        eprintln!("[WARN] failed to write {}: {e}", mcir_path.display());
    }

    if dump_liveness || dump_intervals {
        // --- Dump Liveness Analysis ---
        use regalloc::liveness::{
            LivenessAnalysis, build_live_intervals, format_live_intervals, format_liveness_map,
        };
        for (i, body) in optimized_context.func_bodies.iter().enumerate() {
            let live_map = LivenessAnalysis::new(body).analyze();
            let func_name = optimized_context
                .symbols
                .func_name(i)
                .unwrap_or("<unknown>");
            if dump_liveness {
                print!("{}", format_liveness_map(&live_map, func_name));
            }
            if dump_intervals {
                let intervals = build_live_intervals(body, &live_map);
                print!("{}", format_live_intervals(&intervals, func_name));
            }
        }
    }

    // --- Register Allocation ---
    let target = match args.target {
        TargetKind::Arm64 => targets::arm64::regs::Arm64Target::new(),
    };
    let regalloc_context = regalloc::regalloc(optimized_context, &target);

    if dump_regalloc {
        for (i, alloc_result) in regalloc_context.alloc_results.iter().enumerate() {
            let func_name = regalloc_context.symbols.func_name(i).unwrap_or("<unknown>");
            print!("{}", alloc_result.format_alloc_map(func_name, &target));
        }
    }

    // --- Codegen (Assembly) ---

    let asm = match args.target {
        TargetKind::Arm64 => {
            let mut codegen =
                targets::arm64::Arm64Codegen::from_regalloc_context(&regalloc_context);
            codegen
                .generate()
                .map_err(|e| vec![targets::CodegenError::from(e).into()])?
        }
    };

    if dump_asm {
        println!("ASM:");
        println!("--------------------------------");
        println!("{}", asm);
        println!("--------------------------------");
    }

    Ok(asm)
}

fn format_mcir_body(body: &mcir::FuncBody, name: &str) -> String {
    use crate::mcir::LocalKind;

    let mut params = body
        .locals
        .iter()
        .enumerate()
        .filter_map(|(i, local)| match local.kind {
            LocalKind::Param { index } => Some((index, i, local)),
            _ => None,
        })
        .collect::<Vec<_>>();
    params.sort_by_key(|(index, _, _)| *index);

    let mut param_parts = Vec::new();
    for (index, _, local) in params {
        let ty_str = body.types.type_to_string(local.ty);
        let name = local.name.clone().unwrap_or_else(|| format!("p{}", index));
        param_parts.push(format!("{}: {}", name, ty_str));
    }

    let ret_ty = body
        .types
        .type_to_string(body.locals[body.ret_local.index()].ty);
    let header = format!("fn {}({}) -> {} {{", name, param_parts.join(", "), ret_ty);

    body.to_string().replacen("body {", &header, 1)
}
