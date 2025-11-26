use clap::Parser as ClapParser;

mod analysis;
mod ast;
mod cfg;
mod codegen;
mod context;
mod dataflow;
mod diagnostics;
mod ids;
mod ir;
// mod ir_codegen;
mod lexer;
mod lower;
mod parser;
mod regalloc;
mod resolver;
mod type_check;
mod types;

use crate::codegen::Codegen;
use crate::context::AstContext;
use crate::diagnostics::{CompileError, Span, format_error};
use crate::lexer::{LexError, Lexer, Token};
use crate::lower::lower;
use crate::parser::Parser;
use crate::regalloc::alloc::{RegAlloc, TempAllocMapDisplay};
use crate::regalloc::constraints::analyze_constraints;
use crate::resolver::resolve;
use crate::type_check::type_check;

const SOURCE: &str = r#"
fn main(a: u32, b: u32) -> u32 {
    var x = 10;
    var y = 0;
    while (x > 0) {
        x = x - 1;
        y = y + if (x < 5) { 10 } else { 20 };
    }
    y
}
"#;

#[derive(ClapParser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Comma-separated list of things to dump: ast,defmap,typemap,ir,liveness,regalloc,asm
    #[clap(long)]
    dump: Option<String>,
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
                    CompileError::LexError(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::ParserError(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::ResolveError(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::TypeCheckError(e) => {
                        println!("{}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::LowerError(e) => {
                        println!("{}", format_error(SOURCE, Span::default(), e));
                    }
                    error => {
                        println!("{error:?}");
                    }
                }
            }
        }
    }
}

fn compile(source: &str, args: Args) -> Result<String, Vec<CompileError>> {
    // Parse dump flags from comma-separated list, e.g. --dump ast,ir,liveness
    let mut dump_ast = false;
    let mut dump_def_map = false;
    let mut dump_type_map = false;
    let mut dump_ir = false;
    let mut dump_liveness = false;
    let mut dump_regalloc = false;
    let mut dump_asm = false;

    if let Some(dump) = &args.dump {
        for item in dump.split(',').map(|s| s.trim().to_lowercase()) {
            match item.as_str() {
                "ast" => dump_ast = true,
                "defmap" => dump_def_map = true,
                "typemap" => dump_type_map = true,
                "ir" => dump_ir = true,
                "liveness" => dump_liveness = true,
                "regalloc" => dump_regalloc = true,
                "asm" => dump_asm = true,
                "" => {}
                _ => {
                    eprintln!("[WARN] unknown dump flag: {item}");
                }
            }
        }
    }

    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().map_err(|e| vec![e.into()])?;

    if dump_ast {
        println!("AST:");
        println!("--------------------------------");
        println!("{}", module);
        println!("--------------------------------");
    }

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

    let lowered_context = lower(type_checked_context.clone()).map_err(|e| vec![e.into()])?;
    if dump_ir {
        println!("IR:");
        println!("--------------------------------");
        for func in &lowered_context.ir_funcs {
            println!("{}", func);
            println!("--------------------------------");
        }
    }

    // Liveness analysis
    if dump_liveness {
        use crate::dataflow::liveness::{LiveMapDisplay, LivenessAnalysis};
        for func in &lowered_context.ir_funcs {
            let live_map = LivenessAnalysis::new(func.clone()).analyze();
            println!("Live Map ({}):", func.name);
            println!("--------------------------------");
            println!(
                "{}",
                LiveMapDisplay {
                    func,
                    live_map: &live_map
                }
            );
            println!("--------------------------------");
        }
    }

    // register allocation
    if dump_regalloc {
        for func in &lowered_context.ir_funcs {
            let constraints = analyze_constraints(&func);
            let alloc_result = RegAlloc::new(&func, &constraints).alloc();
            println!("Reg Alloc Map ({}):", func.name);
            println!("--------------------------------");
            println!("{}", TempAllocMapDisplay(&alloc_result.alloc_map));
            println!("--------------------------------");
        }
    }

    let mut codegen = Codegen::new(type_checked_context);
    let asm = codegen.generate().map_err(|e| vec![e.into()])?;

    if dump_asm {
        println!("ASM:");
        println!("--------------------------------");
        println!("{}", asm);
        println!("--------------------------------");
    }

    Ok(asm)
}
