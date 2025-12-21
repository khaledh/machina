use clap::Parser as ClapParser;

mod ast;
mod context;
mod diagnostics;
mod lexer;
mod mcir;
mod nrvo;
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

const SOURCE: &str = r#"
type Point = {
  x: u64,
  y: u64,
}

type Line = {
  start: Point,
  end: Point,
}

fn unpack_point(p: Point) -> (u64, u64) {
    (p.x, p.y)
}

fn add_points(a: Point, b: Point) -> Point {
    let (x1, y1) = unpack_point(a);
    let (x2, y2) = unpack_point(b);
    Point {
        x: x1 + x2,
        y: y1 + y2,
    }
}

fn main() -> u64 {
    let arr = [
        Point { x: 10, y: 20 },
        Point { x: 30, y: 40 },
        Point { x: 50, y: 60 },
    ];
    let sum1 = add_points(arr[0], arr[1]);
    let sum2 = add_points(sum1, arr[2]);
    sum2.x + sum2.y
}
"#;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Comma-separated list of things to dump: ast,defmap,typemap,ir,liveness,intervals,regalloc,asm
    #[clap(long)]
    dump: Option<String>,

    /// Target architecture (e.g. arm64)
    #[clap(long, value_enum, default_value_t = TargetKind::Arm64)]
    target: TargetKind,
}

fn main() {
    let args = Args::parse();
    let output = compile(SOURCE, args);

    match output {
        Ok(asm) => match std::fs::write("output.s", asm) {
            Ok(_) => println!("[SUCCESS] assembly written to output.s"),
            Err(e) => println!("[ERROR] failed to write assembly: {e}"),
        },
        Err(errors) => {
            for error in errors {
                match error {
                    CompileError::Lex(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::Parse(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::Resolve(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::TypeCheck(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::Lower(e) => {
                        println!("{}", format_error(SOURCE, Span::default(), e));
                    }
                    CompileError::Codegen(e) => {
                        println!("{}", format_error(SOURCE, Span::default(), e));
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
    {
        let bodies = &lowered_context.func_bodies;
        if dump_ir {
            println!("MCIR:");
            println!("--------------------------------");
            for body in bodies {
                println!("{}", body);
                println!("--------------------------------");
            }
        }

        if dump_liveness || dump_intervals {
            // --- Dump Liveness Analysis ---
            use regalloc::liveness::{
                build_live_intervals, format_live_intervals, format_liveness_map, LivenessAnalysis,
            };
            for (i, body) in bodies.iter().enumerate() {
                let live_map = LivenessAnalysis::new(body).analyze();
                let func_name = lowered_context.symbols.func_name(i).unwrap_or("<unknown>");
                if dump_liveness {
                    print!("{}", format_liveness_map(&live_map, func_name));
                }
                if dump_intervals {
                    let intervals = build_live_intervals(body, &live_map);
                    print!("{}", format_live_intervals(&intervals, func_name));
                }
            }
        }
    }

    // --- Register Allocation ---
    let target = match args.target {
        TargetKind::Arm64 => targets::arm64::regs::Arm64Target::new(),
    };
    let regalloc_context = regalloc::regalloc(lowered_context, &target);

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
