use clap::Parser as ClapParser;

mod analysis;
mod ast;
mod codegen;
mod context;
mod diagnostics;
mod ids;
mod ir;
mod layout;
mod lexer;
mod lower;
mod mcir;
mod mccodegen;
mod mcregalloc;
mod nrvo;
mod parser;
mod regalloc;
mod resolver;
mod type_check;
mod types;

use crate::context::AstContext;
use crate::diagnostics::{CompileError, Span, format_error};
use crate::lexer::{LexError, Lexer, Token};
use crate::mccodegen::arm64::{Arm64Codegen as McArm64Codegen, McFunction};
use crate::mcregalloc::alloc::RegAlloc as McRegAlloc;
use crate::mcregalloc::constraints::analyze_constraints as analyze_mcir_constraints;
use crate::nrvo::NrvoAnalyzer;
use crate::parser::Parser;
use crate::resolver::resolve;
use crate::type_check::type_check;
use crate::analysis::DefKind;
use std::collections::HashMap;

const SOURCE: &str = r#"
type Point = {
  x: u64,
  y: u64,
}

type Line = {
  start: Point,
  end: Point,
}

fn add_points(a: Point, b: Point) -> Point {
    Point {
        x: a.x + b.x,
        y: a.y + b.y,
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

// const SOURCE2: &str = r#"
// fn main() -> u64 {
//     var x = 20;
//     x = 42;
//     x
// }
// "#;

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
                    CompileError::McLower(e) => {
                        println!("{}", format_error(SOURCE, Span::default(), e));
                    }
                    CompileError::McCodegen(e) => {
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

    if dump_tokens {
        println!("Tokens:");
        println!("--------------------------------");
        for (i, token) in tokens.iter().enumerate() {
            println!("{}: {}", i, token);
        }
        println!("--------------------------------");
    }

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

    let analyzed_context = NrvoAnalyzer::new(type_checked_context).analyze();

    if dump_nrvo {
        println!("NRVO:");
        println!("--------------------------------");
        for def in analyzed_context.def_map.get_nrvo_eligible_defs() {
            println!("{}", def);
        }
        println!("--------------------------------");
    }

    let func_names: Vec<String> = analyzed_context
        .module
        .funcs()
        .iter()
        .map(|f| f.name.clone())
        .collect();

    let def_names: HashMap<_, _> = analyzed_context
        .def_map
        .clone()
        .into_iter()
        .filter(|def| matches!(def.kind, DefKind::Func))
        .map(|def| (def.id, def.name))
        .collect();

    let bodies = mcir::lower_ast::lower_ast(analyzed_context).map_err(|e| vec![e.into()])?;
    if dump_ir {
        println!("MCIR:");
        println!("--------------------------------");
        for body in &bodies {
            println!("{}", body);
            println!("--------------------------------");
        }
    }

    if dump_liveness {
        use mcregalloc::liveness::LivenessAnalysis;
        for (i, body) in bodies.iter().enumerate() {
            let live_map = LivenessAnalysis::new(body).analyze();
            println!("Live Map ({}):", func_names[i]);
            println!("--------------------------------");
            for (bb_idx, live) in live_map.iter().enumerate() {
                println!("  bb{}:", bb_idx);
                print!("    live_in: [");
                let mut live_in: Vec<_> = live.live_in.iter().map(|l| l.0).collect();
                live_in.sort();
                for (idx, id) in live_in.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("%t{}", id);
                }
                println!("]");
                print!("    live_out: [");
                let mut live_out: Vec<_> = live.live_out.iter().map(|l| l.0).collect();
                live_out.sort();
                for (idx, id) in live_out.iter().enumerate() {
                    if idx > 0 {
                        print!(", ");
                    }
                    print!("%t{}", id);
                }
                println!("]");
            }
            println!("--------------------------------");
        }
    }

    let mut alloc_results = Vec::new();
    for body in &bodies {
        let constraints = analyze_mcir_constraints(body);
        let alloc_result = McRegAlloc::new(body, &constraints).alloc();
        alloc_results.push(alloc_result);
    }

    if dump_regalloc {
        for alloc_result in &alloc_results {
            println!("Reg Alloc Map:");
            println!("--------------------------------");
            let mut locals: Vec<_> = alloc_result.alloc_map.iter().collect();
            locals.sort_by_key(|(id, _)| id.0);
            for (id, mapped) in locals {
                match mapped {
                    mcregalloc::MappedLocal::Reg(reg) => {
                        println!("%t{} -> {}", id.0, reg);
                    }
                    mcregalloc::MappedLocal::Stack(slot) => {
                        println!("%t{} -> stack[{}]", id.0, slot.0);
                    }
                    mcregalloc::MappedLocal::StackAddr(slot) => {
                        println!("%t{} -> stack_addr[{}]", id.0, slot.0);
                    }
                }
            }
            println!("--------------------------------");
        }
    }

    let mc_funcs: Vec<McFunction> = bodies
        .iter()
        .zip(func_names.iter())
        .zip(alloc_results.iter())
        .map(|((body, name), alloc)| McFunction {
            name: name.clone(),
            body,
            alloc,
        })
        .collect();

    let mut codegen = McArm64Codegen::new(mc_funcs, &def_names);
    let asm = codegen.generate().map_err(|e| vec![e.into()])?;

    if dump_asm {
        println!("ASM:");
        println!("--------------------------------");
        println!("{}", asm);
        println!("--------------------------------");
    }

    Ok(asm)
}
