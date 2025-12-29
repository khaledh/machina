use super::*;
use crate::ast::{
    BlockItem, Expr, ExprKind, Function, MatchPattern, Module, PatternKind, StmtExpr, StmtExprKind,
    StringTag, TypeDeclKind, TypeExprKind,
};
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

fn block_parts(expr: &Expr) -> (&[BlockItem], Option<&Expr>) {
    match &expr.kind {
        ExprKind::Block { items, tail } => (items, tail.as_deref()),
        _ => panic!("Expected Block"),
    }
}

fn block_tail(expr: &Expr) -> &Expr {
    let (_, tail) = block_parts(expr);
    tail.expect("Expected block to have a tail expr")
}

fn block_stmt_at(items: &[BlockItem], index: usize) -> &StmtExpr {
    match &items[index] {
        BlockItem::Stmt(stmt) => stmt,
        BlockItem::Expr(_) => panic!("Expected stmt block item"),
    }
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

    assert_eq!(func.sig.name, "test");
    match &func.sig.return_type.kind {
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

    match &func.sig.return_type.kind {
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

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    if let ExprKind::ArrayIndex { target, indices } = &tail.kind {
        // Check target is Var
        match &target.kind {
            ExprKind::Var(name) => assert_eq!(name, "arr"),
            _ => panic!("Expected Var"),
        }

        // Check we have 2 indices
        assert_eq!(indices.len(), 2);

        // Check indices are literals 1 and 0
        match &indices[0].kind {
            ExprKind::IntLit(val) => assert_eq!(*val, 1),
            _ => panic!("Expected IntLit"),
        }
        match &indices[1].kind {
            ExprKind::IntLit(val) => assert_eq!(*val, 0),
            _ => panic!("Expected IntLit"),
        }
    } else {
        panic!("Expected ArrayIndex expression");
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

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
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
}

#[test]
fn test_parse_typed_array_literal() {
    let source = r#"
        fn test() -> u64 {
            let arr = u8[1, 2, 3];
            arr[0]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::TypedArrayLit { elem_ty, elems } = &value.kind {
            match &elem_ty.kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u8"),
                _ => panic!("Expected named type"),
            }
            assert_eq!(elems.len(), 3);
            for elem in elems {
                assert!(matches!(elem.kind, ExprKind::IntLit(_)));
            }
        } else {
            panic!("Expected TypedArrayLit");
        }
    } else {
        panic!("Expected Let");
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

    assert_eq!(func.sig.name, "test");
    match &func.sig.return_type.kind {
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

    match &func.sig.return_type.kind {
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

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            assert_eq!(enum_name, "Color");
            assert_eq!(variant, "Green");
            assert!(payload.is_empty());
        }
        _ => panic!("Expected enum variant expr"),
    }
}

#[test]
fn test_parse_enum_type_decl_with_payload() {
    let source = r#"
        type Option = None | Some(u64)

        fn main() -> u64 {
            0
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_decls = module.type_decls();
    assert_eq!(type_decls.len(), 1);

    let type_decl = type_decls[0];
    assert_eq!(type_decl.name, "Option");
    match &type_decl.kind {
        TypeDeclKind::Enum { variants } => {
            assert_eq!(variants.len(), 2);
            assert_eq!(variants[0].name, "None");
            assert!(variants[0].payload.is_empty());
            assert_eq!(variants[1].name, "Some");
            assert_eq!(variants[1].payload.len(), 1);
            match &variants[1].payload[0].kind {
                TypeExprKind::Named(name) => assert_eq!(name, "u64"),
                _ => panic!("Expected named payload type"),
            }
        }
        _ => panic!("Expected enum type decl"),
    }
}

#[test]
fn test_parse_enum_variant_expr_with_payload() {
    let source = r#"
        type Option = None | Some(u64)

        fn main() -> Option {
            Option::Some(42)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
        } => {
            assert_eq!(enum_name, "Option");
            assert_eq!(variant, "Some");
            assert_eq!(payload.len(), 1);
            match &payload[0].kind {
                ExprKind::IntLit(value) => assert_eq!(*value, 42),
                _ => panic!("Expected integer payload"),
            }
        }
        _ => panic!("Expected enum variant expr"),
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

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    match &tail.kind {
        ExprKind::StructUpdate { target, fields } => {
            match &target.kind {
                ExprKind::Var(name) => assert_eq!(name, "p"),
                _ => panic!("Expected StructUpdate target to be Var"),
            }
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name, "color");
            match &fields[0].value.kind {
                ExprKind::EnumVariant {
                    enum_name,
                    variant,
                    payload,
                } => {
                    assert_eq!(enum_name, "Color");
                    assert_eq!(variant, "Green");
                    assert!(payload.is_empty());
                }
                _ => panic!("Expected enum variant value"),
            }
        }
        _ => panic!("Expected StructUpdate expression"),
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

    let tail = block_tail(&func.body);
    if let ExprKind::TupleLit(fields) = &tail.kind {
        assert_eq!(fields.len(), 2);

        match &fields[0].kind {
            ExprKind::IntLit(val) => assert_eq!(*val, 42),
            _ => panic!("Expected IntLit"),
        }

        match &fields[1].kind {
            ExprKind::BoolLit(val) => assert_eq!(*val, true),
            _ => panic!("Expected BoolLit"),
        }
    } else {
        panic!("Expected TupleLit");
    }
}

#[test]
fn test_parse_string_literal_ascii() {
    let source = r#"
        fn main() {
            "hello"
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::StringLit { value, tag } => {
            assert_eq!(value, "hello");
            assert_eq!(*tag, StringTag::Ascii);
        }
        _ => panic!("Expected string literal"),
    }
}

#[test]
fn test_parse_string_literal_utf8() {
    let source = r#"
        fn main() {
            "café"
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::StringLit { value, tag } => {
            assert_eq!(value, "café");
            assert_eq!(*tag, StringTag::Utf8);
        }
        _ => panic!("Expected string literal"),
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

    let tail = block_tail(&func.body);
    if let ExprKind::TupleLit(fields) = &tail.kind {
        assert_eq!(fields.len(), 2);
    } else {
        panic!("Expected TupleLit");
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

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    if let ExprKind::TupleField { target, index } = &tail.kind {
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

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    if let ExprKind::TupleField { target, index } = &tail.kind {
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

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    if let ExprKind::ArrayIndex { target, indices } = &tail.kind {
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

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { pattern, value, .. } = &stmt.kind {
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

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { pattern, .. } = &stmt.kind {
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

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { pattern, .. } = &stmt.kind {
        match &pattern.kind {
            PatternKind::Tuple { patterns } => {
                assert_eq!(patterns.len(), 2);
            }
            _ => panic!("Expected Tuple pattern"),
        }
    } else {
        panic!("Expected Let");
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

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { pattern, .. } = &stmt.kind {
        // Should be an Ident pattern, not a Tuple pattern
        match &pattern.kind {
            PatternKind::Ident { name } => assert_eq!(name, "a"),
            _ => panic!("Expected Ident pattern (parenthesized), got {:?}", pattern),
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_match_expr_enum_variants() {
    let source = r#"
        type Color = Red(u64) | Green | Blue(u64)

        fn test(c: Color) -> u64 {
            match c {
                Red(x,) => x,
                Color::Blue(y,) => y,
                _ => 0,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::Match { scrutinee, arms } => {
            match &scrutinee.kind {
                ExprKind::Var(name) => assert_eq!(name, "c"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 3);

            match &arms[0].pattern {
                MatchPattern::EnumVariant {
                    enum_name,
                    variant_name,
                    bindings,
                    ..
                } => {
                    assert!(enum_name.is_none());
                    assert_eq!(variant_name, "Red");
                    assert_eq!(bindings.len(), 1);
                    assert_eq!(bindings[0].name, "x");
                }
                _ => panic!("Expected enum variant pattern in arm 0"),
            }

            match &arms[1].pattern {
                MatchPattern::EnumVariant {
                    enum_name,
                    variant_name,
                    bindings,
                    ..
                } => {
                    assert_eq!(enum_name.as_deref(), Some("Color"));
                    assert_eq!(variant_name, "Blue");
                    assert_eq!(bindings.len(), 1);
                    assert_eq!(bindings[0].name, "y");
                }
                _ => panic!("Expected enum variant pattern in arm 1"),
            }

            assert!(matches!(arms[2].pattern, MatchPattern::Wildcard { .. }));
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_char_literal() {
    let source = r#"
        fn test() -> char {
            'a'
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::CharLit(value) => assert_eq!(*value, 'a'),
        _ => panic!("Expected CharLit"),
    }
}

#[test]
fn test_parse_for_range_loop() {
    let source = r#"
        fn test() -> u64 {
            for i in 0..3 { i; }
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, tail) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::For {
        pattern,
        iter,
        body,
    } = &stmt.kind
    {
        match &pattern.kind {
            PatternKind::Ident { name } => assert_eq!(name, "i"),
            _ => panic!("Expected ident pattern"),
        }
        match &iter.kind {
            ExprKind::Range { start, end } => {
                assert_eq!(*start, 0);
                assert_eq!(*end, 3);
            }
            _ => panic!("Expected range iterator"),
        }
        assert!(matches!(body.kind, ExprKind::Block { .. }));
    } else {
        panic!("Expected for loop");
    }

    let tail = tail.expect("Expected block to have a tail expr");
    assert!(matches!(tail.kind, ExprKind::IntLit(0)));
}

#[test]
fn test_parse_for_array_loop() {
    let source = r#"
        fn test() -> u64 {
            for x in [1, 2, 3] { x; }
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, tail) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::For {
        pattern,
        iter,
        body,
    } = &stmt.kind
    {
        match &pattern.kind {
            PatternKind::Ident { name } => assert_eq!(name, "x"),
            _ => panic!("Expected ident pattern"),
        }
        assert!(matches!(iter.kind, ExprKind::ArrayLit(_)));
        assert!(matches!(body.kind, ExprKind::Block { .. }));
    } else {
        panic!("Expected for loop");
    }

    let tail = tail.expect("Expected block to have a tail expr");
    assert!(matches!(tail.kind, ExprKind::IntLit(0)));
}
