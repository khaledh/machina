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
mod lexer;
mod lower;
mod parser;
mod resolver;
mod type_check;
mod types;

use crate::codegen::Codegen;
use crate::context::AstContext;
use crate::diagnostics::{CompileError, Span, format_error};
use crate::lexer::{LexError, Lexer, Token};
use crate::lower::lower;
use crate::parser::Parser;
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
    #[clap(long)]
    dump_ast: bool,

    #[clap(long)]
    dump_def_map: bool,

    #[clap(long)]
    dump_type_map: bool,

    #[clap(long)]
    dump_ir: bool,

    #[clap(long)]
    dump_asm: bool,
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
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().map_err(|e| vec![e.into()])?;

    if args.dump_ast {
        println!("AST:");
        println!("--------------------------------");
        println!("{}", module);
        println!("--------------------------------");
    }

    let ast_contet = AstContext::new(module);

    let resolved_context = resolve(ast_contet).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    if args.dump_def_map {
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

    if args.dump_type_map {
        println!("Type Map:");
        println!("--------------------------------");
        println!("{}", type_checked_context.type_map);
        println!("--------------------------------");
    }

    let lowered_context = lower(type_checked_context.clone()).map_err(|e| vec![e.into()])?;
    if args.dump_ir {
        println!("IR:");
        println!("--------------------------------");
        for func in &lowered_context.ir_funcs {
            println!("{}", func);
            println!("--------------------------------");
        }
    }

    let mut codegen = Codegen::new(type_checked_context);
    let asm = codegen.generate().map_err(|e| vec![e.into()])?;

    Ok(asm)
}
