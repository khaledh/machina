mod ast;
mod codegen;
mod diagnostics;
mod lexer;
mod parser;
mod sem_analysis;
mod type_check;

use crate::codegen::CodegenError;
use crate::lexer::{LexError, Lexer, Token};
use crate::parser::{Parser, ParserError};
use crate::sem_analysis::SemError;
use crate::type_check::TypeCheckError;
use thiserror::Error;

#[derive(Debug, Error)]
enum CompileError {
    #[error(transparent)]
    LexError(#[from] LexError),

    #[error(transparent)]
    ParserError(#[from] ParserError),

    #[error("Semantic analysis errors: {0:#?}")]
    SemError(Vec<SemError>),

    #[error("Type check errors: {0:#?}")]
    TypeCheckError(Vec<TypeCheckError>),

    #[error(transparent)]
    CodegenError(#[from] CodegenError),
}

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
                println!("{error}");
            }
        }
    }
}

fn compile(source: &str) -> Result<String, Vec<CompileError>> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokens()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().map_err(|e| vec![e.into()])?;
    // println!("AST:\n{:#?}", module);

    let mut sem = sem_analysis::SemanticAnalyzer::new();
    sem.analyze(&module)
        .map_err(|errs| vec![CompileError::SemError(errs)])?;

    let mut type_checker = type_check::TypeChecker::new();
    type_checker
        .type_check(&module)
        .map_err(|errs| vec![CompileError::TypeCheckError(errs)])?;

    let mut codegen = codegen::Codegen::new(&module);
    let asm = codegen.generate().map_err(|e| vec![e.into()])?;

    Ok(asm)
}
