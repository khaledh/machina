use crate::context::AstContext;
use crate::desugar;
use crate::hir::visit::{Visitor, walk_expr};
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::Parser;
use crate::resolve::resolve;

fn desugar_source(source: &str) -> crate::context::HirContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");

    let ast_context = AstContext::new(module);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    desugar::desugar(resolved_context)
}

struct ExprCounter {
    count: usize,
}

impl Visitor for ExprCounter {
    fn visit_expr(&mut self, expr: &crate::ast::Expr) {
        self.count += 1;
        walk_expr(self, expr);
    }
}

#[test]
fn test_desugar_preserves_decl_count() {
    let source = r#"
        type Pair = { a: u64, b: u64 }

        fn add(a: u64, b: u64) -> u64 {
            a + b
        }
    "#;

    let ctx = desugar_source(source);
    assert_eq!(ctx.module.decls.len(), 2);
}

#[test]
fn test_hir_visitor_walks_exprs() {
    let source = r#"
        fn add(a: u64, b: u64) -> u64 {
            a + b
        }
    "#;

    let ctx = desugar_source(source);
    let mut counter = ExprCounter { count: 0 };
    counter.visit_module(&ctx.module);
    assert!(counter.count > 0, "expected to visit expressions");
}
