use super::*;
use crate::ast::{ExprKind, Function, Module, PatternKind, TypeDeclKind, TypeExprKind};
use crate::lexer::{LexError, Lexer, Token};

fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    parser.parse()
}

fn parse_source(source: &str) -> Result<Vec<Function>, ParseError> {
    let module = parse_module(source)?;
    Ok(module.funcs().into_iter().cloned().collect())
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
    match &func.return_type.kind {
        TypeExprKind::Array { elem_ty, dims } => {
            match &elem_ty.kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
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

    match &func.return_type.kind {
        TypeExprKind::Array { elem_ty, dims } => {
            match &elem_ty.kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
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
        if let ExprKind::ArrayIndex { target, indices } = &exprs[1].kind {
            // Check target is Var
            match &target.kind {
                ExprKind::Var(name) => assert_eq!(name, "arr"),
                _ => panic!("Expected Var"),
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
            panic!("Expected ArrayIndex expression");
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
        if let ExprKind::LetBind { value, .. } = &exprs[0].kind {
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

#[test]
fn test_parse_tuple_type() {
    let source = r#"
        fn test() -> (u64, bool) {
            (42, true)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    assert_eq!(func.name, "test");
    match &func.return_type.kind {
        TypeExprKind::Tuple { fields } => {
            assert_eq!(fields.len(), 2);
            match &fields[0].kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            match &fields[1].kind {
                TypeExprKind::Named(name) => assert_eq!(name, "bool"),
                _ => panic!("Expected named type"),
            }
        }
        _ => panic!("Expected tuple type"),
    }
}

#[test]
fn test_parse_tuple_type_nested() {
    let source = r#"
        fn test() -> (u64, (bool, u64)) {
            (42, (true, 10))
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.return_type.kind {
        TypeExprKind::Tuple { fields } => {
            assert_eq!(fields.len(), 2);
            match &fields[0].kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            match &fields[1].kind {
                TypeExprKind::Tuple { fields: inner } => {
                    assert_eq!(inner.len(), 2);
                    match &inner[0].kind {
                        TypeExprKind::Named(name) => assert_eq!(name, "bool"),
                        _ => panic!("Expected named type"),
                    }
                    match &inner[1].kind {
                        TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                        _ => panic!("Expected named type"),
                    }
                }
                _ => panic!("Expected nested tuple type"),
            }
        }
        _ => panic!("Expected tuple type"),
    }
}

#[test]
fn test_parse_enum_type_decl() {
    let source = r#"
        type Color = Red | Green | Blue

        fn main() -> u64 {
            0
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_decls = module.type_decls();
    assert_eq!(type_decls.len(), 1);

    let type_decl = type_decls[0];
    assert_eq!(type_decl.name, "Color");
    match &type_decl.kind {
        TypeDeclKind::Enum { variants } => {
            let names = variants.iter().map(|v| v.name.as_str()).collect::<Vec<_>>();
            assert_eq!(names, vec!["Red", "Green", "Blue"]);
        }
        _ => panic!("Expected enum type decl"),
    }
}

#[test]
fn test_parse_enum_variant_expr() {
    let source = r#"
        type Color = Red | Green

        fn main() -> Color {
            Color::Green
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        assert_eq!(exprs.len(), 1);
        match &exprs[0].kind {
            ExprKind::EnumVariant { enum_name, variant } => {
                assert_eq!(enum_name, "Color");
                assert_eq!(variant, "Green");
            }
            _ => panic!("Expected enum variant expr"),
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_struct_update_expr() {
    let source = r#"
        type Color = Red | Green
        type Point = { x: u64, y: u64, color: Color }

        fn main() -> Point {
            let p = Point { x: 1, y: 2, color: Color::Red };
            { p | color: Color::Green }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        let last = exprs.last().expect("Expected block to have a tail expr");
        match &last.kind {
            ExprKind::StructUpdate { target, fields } => {
                match &target.kind {
                    ExprKind::Var(name) => assert_eq!(name, "p"),
                    _ => panic!("Expected StructUpdate target to be Var"),
                }
                assert_eq!(fields.len(), 1);
                assert_eq!(fields[0].name, "color");
                match &fields[0].value.kind {
                    ExprKind::EnumVariant { enum_name, variant } => {
                        assert_eq!(enum_name, "Color");
                        assert_eq!(variant, "Green");
                    }
                    _ => panic!("Expected enum variant value"),
                }
            }
            _ => panic!("Expected StructUpdate expression"),
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_literal() {
    let source = r#"
        fn test() -> (u64, bool) {
            (42, true)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::TupleLit(fields) = &exprs[0].kind {
            assert_eq!(fields.len(), 2);

            match &fields[0].kind {
                ExprKind::UInt64Lit(val) => assert_eq!(*val, 42),
                _ => panic!("Expected UInt64Lit"),
            }

            match &fields[1].kind {
                ExprKind::BoolLit(val) => assert_eq!(*val, true),
                _ => panic!("Expected BoolLit"),
            }
        } else {
            panic!("Expected TupleLit");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_literal_trailing_comma() {
    let source = r#"
        fn test() -> (u64, bool) {
            (42, true,)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::TupleLit(fields) = &exprs[0].kind {
            assert_eq!(fields.len(), 2);
        } else {
            panic!("Expected TupleLit");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_field_access() {
    let source = r#"
        fn test() -> u64 {
            let t = (42, true);
            t.0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        // Second expression is the field access
        if let ExprKind::TupleField { target, index } = &exprs[1].kind {
            // Check target is Var
            match &target.kind {
                ExprKind::Var(name) => assert_eq!(name, "t"),
                _ => panic!("Expected Var"),
            }

            // Check index is 0
            assert_eq!(*index, 0);
        } else {
            panic!("Expected TupleField");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_field_access_chained() {
    let source = r#"
        fn test() -> bool {
            let t = (42, (true, 10));
            t.1.0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        // Second expression should be field access of field access
        if let ExprKind::TupleField { target, index } = &exprs[1].kind {
            assert_eq!(*index, 0);

            // Inner target should also be field access
            if let ExprKind::TupleField {
                target: inner_target,
                index: inner_index,
            } = &target.kind
            {
                assert_eq!(*inner_index, 1);

                match &inner_target.kind {
                    ExprKind::Var(name) => assert_eq!(name, "t"),
                    _ => panic!("Expected Var"),
                }
            } else {
                panic!("Expected nested TupleField");
            }
        } else {
            panic!("Expected TupleField");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_with_array_indexing() {
    let source = r#"
        fn test() -> u64 {
            let t = ([1, 2, 3], 42);
            t.0[2]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        // Second expression should be index of field access
        if let ExprKind::ArrayIndex { target, indices } = &exprs[1].kind {
            assert_eq!(indices.len(), 1);

            // Target should be field access
            if let ExprKind::TupleField {
                target: field_target,
                index,
            } = &target.kind
            {
                assert_eq!(*index, 0);

                match &field_target.kind {
                    ExprKind::Var(name) => assert_eq!(name, "t"),
                    _ => panic!("Expected Var"),
                }
            } else {
                panic!("Expected TupleField");
            }
        } else {
            panic!("Expected ArrayIndex");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_pattern() {
    let source = r#"
        fn test() -> u64 {
            let (a, b) = (42, true);
            a
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::LetBind { pattern, value, .. } = &exprs[0].kind {
            // Check pattern is a tuple pattern
            match &pattern.kind {
                PatternKind::Tuple { patterns } => {
                    assert_eq!(patterns.len(), 2);

                    // First pattern should be identifier "a"
                    match &patterns[0].kind {
                        PatternKind::Ident { name } => assert_eq!(name, "a"),
                        _ => panic!("Expected Ident pattern"),
                    }

                    // Second pattern should be identifier "b"
                    match &patterns[1].kind {
                        PatternKind::Ident { name } => assert_eq!(name, "b"),
                        _ => panic!("Expected Ident pattern"),
                    }
                }
                _ => panic!("Expected Tuple pattern"),
            }

            // Check value is a tuple literal
            match &value.kind {
                ExprKind::TupleLit(fields) => {
                    assert_eq!(fields.len(), 2);
                }
                _ => panic!("Expected TupleLit"),
            }
        } else {
            panic!("Expected Let");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_pattern_nested() {
    let source = r#"
        fn test() -> u64 {
            let (a, (b, c)) = (1, (2, 3));
            a
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::LetBind { pattern, .. } = &exprs[0].kind {
            match &pattern.kind {
                PatternKind::Tuple { patterns } => {
                    assert_eq!(patterns.len(), 2);

                    // First pattern should be identifier "a"
                    match &patterns[0].kind {
                        PatternKind::Ident { name } => assert_eq!(name, "a"),
                        _ => panic!("Expected Ident pattern"),
                    }

                    // Second pattern should be a nested tuple pattern
                    match &patterns[1].kind {
                        PatternKind::Tuple { patterns: inner } => {
                            assert_eq!(inner.len(), 2);
                            match &inner[0].kind {
                                PatternKind::Ident { name } => assert_eq!(name, "b"),
                                _ => panic!("Expected Ident pattern"),
                            }
                            match &inner[1].kind {
                                PatternKind::Ident { name } => assert_eq!(name, "c"),
                                _ => panic!("Expected Ident pattern"),
                            }
                        }
                        _ => panic!("Expected nested Tuple pattern"),
                    }
                }
                _ => panic!("Expected Tuple pattern"),
            }
        } else {
            panic!("Expected Let");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_tuple_pattern_trailing_comma() {
    let source = r#"
        fn test() -> u64 {
            let (a, b,) = (42, true);
            a
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::LetBind { pattern, .. } = &exprs[0].kind {
            match &pattern.kind {
                PatternKind::Tuple { patterns } => {
                    assert_eq!(patterns.len(), 2);
                }
                _ => panic!("Expected Tuple pattern"),
            }
        } else {
            panic!("Expected Let");
        }
    } else {
        panic!("Expected Block");
    }
}

#[test]
fn test_parse_parenthesized_pattern() {
    // (a) should be parsed as just `a`, not a tuple pattern
    let source = r#"
        fn test() -> u64 {
            let (a) = 42;
            a
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    if let ExprKind::Block(exprs) = &func.body.kind {
        if let ExprKind::LetBind { pattern, .. } = &exprs[0].kind {
            // Should be an Ident pattern, not a Tuple pattern
            match &pattern.kind {
                PatternKind::Ident { name } => assert_eq!(name, "a"),
                _ => panic!("Expected Ident pattern (parenthesized), got {:?}", pattern),
            }
        } else {
            panic!("Expected Let");
        }
    } else {
        panic!("Expected Block");
    }
}
