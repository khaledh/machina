mod analysis;
mod ast;
mod codegen;
mod context;
mod diagnostics;
mod ids;
mod lexer;
mod parser;
mod resolver;
mod type_check;
mod types;

use crate::codegen::Codegen;
use crate::context::Context;
use crate::diagnostics::{CompileError, format_error};
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::Parser;
use crate::resolver::resolve;
use crate::type_check::type_check;

const SOURCE: &str = r#"
fn inc(a: u32) -> u32 {
    a + 1
}

fn ge(a: u32, b: u32) -> bool {
    a >= b
}

fn main() -> u32 {
    if ge(inc(41), 42) { 42 / 2 } else { 99 }
}
"#;

fn main() {
    let output = compile(SOURCE);

    match output {
        Ok(asm) => match std::fs::write("output.s", asm) {
            Ok(_) => println!("[SUCCESS] assembly written to output.s"),
            Err(e) => println!("[ERROR] failed to write assembly: {e}"),
        },
        Err(errors) => {
            for error in errors {
                match error {
                    CompileError::LexError(e) => {
                        println!("[ERROR] {}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::ParserError(e) => {
                        println!("[ERROR] {}", format_error(SOURCE, e.span(), e));
                    }
                    CompileError::ResolveError(e) => {
                        println!("[ERROR] {}", format_error(SOURCE, e.span(), e));
                    }
                    error => {
                        println!("[ERROR] {error:?}");
                    }
                }
            }
        }
    }
}

fn compile(source: &str) -> Result<String, Vec<CompileError>> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().map_err(|e| vec![e.into()])?;

    let context = Context::new(module);

    let resolved_context = resolve(context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    let type_checked_context =
        type_check(resolved_context).map_err(|errs| vec![CompileError::TypeCheckError(errs)])?;

    let mut codegen = Codegen::new(type_checked_context);
    let asm = codegen.generate().map_err(|e| vec![e.into()])?;

    Ok(asm)
}
