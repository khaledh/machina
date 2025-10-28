use crate::lexer::Lexer;

mod ast;
mod codegen;
mod lexer;
mod parser;

const SOURCE: &str = r#"
fn main() -> u32 {
    1 + 2 * 3
}
"#;

fn main() {
    let lexer = Lexer::new(SOURCE);
    let tokens = lexer.tokens();

    let mut parser = parser::Parser::new(tokens);
    let function = parser.parse();
    println!("{:#?}", function);

    let codegen = codegen::Codegen::new(function);
    let asm = codegen.generate();
    std::fs::write("output.s", asm).unwrap();
    println!("Assembly written to output.s");
}
