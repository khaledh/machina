use crate::lexer::Lexer;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod sem_analysis;
mod type_check;

const SOURCE: &str = r#"
fn inc(a: u32) -> u32 {
    a + 1
}

fn ge(a: u32, b: u32) -> bool {
    a >= b
}

fn main() -> u32 {
    if ge(inc(41), 42) { 42 } else { 99 }
}
"#;

fn main() {
    let output = compile(SOURCE);

    match output {
        Ok(asm) => {
            std::fs::write("output.s", asm).unwrap();
            println!("Assembly written to output.s");
        }
        Err(errors) => {
            println!("Errors:\n{:#?}", errors);
        }
    }
}

fn compile(source: &str) -> Result<String, Vec<String>> {
    let lexer = Lexer::new(source);
    let tokens = lexer.tokens();

    let module = parser::parse_tokens(tokens);
    println!("Module:\n{:#?}", module);

    let mut sem = sem_analysis::SemanticAnalyzer::new();
    sem.analyze(&module)?;

    let mut type_checker = type_check::TypeChecker::new();
    type_checker.type_check(&module)?;

    let mut codegen = codegen::Codegen::new(&module);
    let asm = codegen.generate();

    Ok(asm)
}
