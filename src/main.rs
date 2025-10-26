use crate::lexer::Lexer;

mod ast;
mod lexer;
mod parser;

const SOURCE: &str = r#"
fn main() -> u32 {
    42
}
"#;

fn main() {
    let lexer = Lexer::new(SOURCE);
    let tokens = lexer.tokens();

    let mut parser = parser::Parser::new(tokens);
    let function = parser.parse();
    println!("{:#?}", function);
}
