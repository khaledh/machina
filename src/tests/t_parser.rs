use super::*;
use crate::ast::{ExprKind, Function};
use crate::lexer::{LexError, Lexer, Token};
use crate::types::Type;

fn parse_source(source: &str) -> Result<Vec<Function>, ParseError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse()?;
    Ok(module.funcs)
}

#[test]
fn test_parse_multidim_array_type() {
    let source = r#"
        fn test() -> u64[2, 3] {
            [[1, 2, 3], [4, 5, 6]]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    assert_eq!(func.name, "test");
    match &func.return_type {
        Type::Array { elem_ty, dims } => {
            assert_eq!(**elem_ty, Type::UInt64);
            assert_eq!(dims, &vec![2, 3]);
        }
        _ => panic!("Expected array type"),
    }
}

#[test]
fn test_parse_multidim_array_type_3d() {
    let source = r#"
        fn test() -> u64[2, 3, 4] {
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.return_type {
        Type::Array { elem_ty, dims } => {
            assert_eq!(**elem_ty, Type::UInt64);
            assert_eq!(dims, &vec![2, 3, 4]);
        }
        _ => panic!("Expected array type"),
    }
}

#[test]
fn test_parse_multi_index_expr() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2], [3, 4]];
            arr[1, 0]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    // Body is a block
    if let ExprKind::Block(exprs) = &func.body.kind {
        // Second expression is the index
        if let ExprKind::Index { target, indices } = &exprs[1].kind {
            // Check target is VarRef
            match &target.kind {
                ExprKind::VarRef(name) => assert_eq!(name, "arr"),
                _ => panic!("Expected VarRef"),
            }

            // Check we have 2 indices
            assert_eq!(indices.len(), 2);

            // Check indices are literals 1 and 0
            match &indices[0].kind {
                ExprKind::UInt64Lit(val) => assert_eq!(*val, 1),
                _ => panic!("Expected UInt64Lit"),
            }
            match &indices[1].kind {
                ExprKind::UInt64Lit(val) => assert_eq!(*val, 0),
                _ => panic!("Expected UInt64Lit"),
            }
        } else {
            panic!("Expected Index expression");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_nested_array_literal() {
    let source = r#"
        fn test() -> u64 {
            let arr = [[1, 2, 3], [4, 5, 6]];
            arr[0, 0]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        // First expression is the let binding
        if let ExprKind::Let { value, .. } = &exprs[0].kind {
            // Value should be an ArrayLit
            if let ExprKind::ArrayLit(outer_elems) = &value.kind {
                assert_eq!(outer_elems.len(), 2);

                // Each element should be an ArrayLit
                for elem in outer_elems {
                    match &elem.kind {
                        ExprKind::ArrayLit(inner_elems) => {
                            assert_eq!(inner_elems.len(), 3);
                        }
                        _ => panic!("Expected nested ArrayLit"),
                    }
                }
            } else {
                panic!("Expected ArrayLit");
            }
        } else {
            panic!("Expected Let");
        }
    } else {
        panic!("Expected Block");
    }
}
