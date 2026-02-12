use super::*;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;

fn resolve_source(source: &str) -> crate::core::context::ResolvedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("failed to tokenize");
    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("failed to parse");
    let id_gen = parser.into_id_gen();
    let parsed = ParsedContext::new(module, id_gen);
    resolve(parsed).expect("failed to resolve")
}

#[test]
fn test_view_resolver_is_deterministic_for_recursive_nominals() {
    let ctx = resolve_source(
        r#"
        type Link = None | Some(Node^)
        type Node = { value: u64, next: Link }
        fn main() { () }
        "#,
    );
    let link_def_id = ctx
        .def_table
        .lookup_type_def_id("Link")
        .expect("expected Link type def");
    let node_def_id = ctx
        .def_table
        .lookup_type_def_id("Node")
        .expect("expected Node type def");
    let link_key = NominalKey::new(link_def_id, Vec::new());
    let node_key = NominalKey::new(node_def_id, Vec::new());

    let mut first = TypeViewResolver::new(&ctx.def_table, &ctx.module);
    let node_first = first.view_of_key(&node_key).expect("expected Node view");
    let link_first = first.view_of_key(&link_key).expect("expected Link view");

    let mut second = TypeViewResolver::new(&ctx.def_table, &ctx.module);
    let link_second = second.view_of_key(&link_key).expect("expected Link view");
    let node_second = second.view_of_key(&node_key).expect("expected Node view");

    assert_eq!(node_first, node_second);
    assert_eq!(link_first, link_second);
    match node_first {
        TypeView::Struct(struct_view) => {
            assert_eq!(struct_view.state, ExpansionState::Expanded);
            assert_eq!(struct_view.fields.len(), 2);
        }
        other => panic!("expected struct view for Node, got {other:?}"),
    }
    match link_first {
        TypeView::Enum(enum_view) => {
            assert_eq!(enum_view.state, ExpansionState::Expanded);
            assert_eq!(enum_view.variants.len(), 2);
        }
        other => panic!("expected enum view for Link, got {other:?}"),
    }
}
