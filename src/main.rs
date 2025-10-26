use crate::lexer::Lexer;

mod lexer;

const SOURCE: &str = r#"
fn main() -> u32 {
    42
}
"#;

fn main() {
    let mut lexer = Lexer::new(SOURCE);
    while let token = lexer.next_token()
        && token != lexer::TokenKind::Eof
    {
        println!("{:?}", token);
    }
}
