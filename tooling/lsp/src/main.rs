use std::io;

fn main() {
    if let Err(error) = machina_lsp::server::run_stdio(io::stdin(), io::stdout()) {
        eprintln!("machina-lsp: {error}");
        std::process::exit(1);
    }
}
