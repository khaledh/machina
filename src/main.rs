use crate::lexer::Lexer;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;
mod type_check;

const SOURCE: &str = r#"
fn main() -> u32 {
    let x = 42;
    if x <= 100 { x } else { 100 }
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

    let mut parser = parser::Parser::new(tokens);
    let function = parser.parse();
    println!("AST:\n{:#?}", function);

    let mut semantic = semantic::SemanticAnalyzer::new();
    let symbols = semantic.analyze(&function)?;
    println!("Symbols:\n{:#?}", symbols);

    let mut type_checker = type_check::TypeChecker::new();
    let return_type = type_checker.type_check(&function)?;
    println!("Return type: {:#?}", return_type);

    let codegen = codegen::Codegen::new(function, symbols);
    let asm = codegen.generate();

    Ok(asm)
}
