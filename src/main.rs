use crate::lexer::Lexer;

mod ast;
mod codegen;
mod lexer;
mod parser;
mod semantic;

const SOURCE: &str = r#"
fn main() -> bool {
    if 5 < 10 { 42 } else { 99 }
}
"#;

fn main() {
    let lexer = Lexer::new(SOURCE);
    let tokens = lexer.tokens();

    let mut parser = parser::Parser::new(tokens);
    let function = parser.parse();
    println!("{:#?}", function);

    let mut semantic = semantic::SemanticAnalyzer::new();
    let symbols = semantic.analyze(&function);

    match symbols {
        Err(errors) => {
            println!("Semantic errors:");
            for error in errors {
                println!("  - {error}");
            }
        }

        Ok(symbols) => {
            println!("{:#?}", symbols);
            let codegen = codegen::Codegen::new(function, symbols);
            let asm = codegen.generate();
            std::fs::write("output.s", asm).unwrap();
            println!("Assembly written to output.s");
        }
    }
}
