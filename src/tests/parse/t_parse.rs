use super::*;
use crate::core::lexer::{LexError, Lexer, Token, TokenKind as TK};
use crate::core::parse::ParserOptions;
use crate::core::tree::RefinementKind;

fn parse_module(source: &str) -> Result<Module, ParseError> {
    parse_module_with_options(source, ParserOptions::default())
}

fn parse_module_with_options(source: &str, options: ParserOptions) -> Result<Module, ParseError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new_with_id_gen_and_options(&tokens, NodeIdGen::new(), options);
    parser.parse()
}

fn parse_source(source: &str) -> Result<Vec<FuncDef>, ParseError> {
    let module = parse_module(source)?;
    Ok(module.func_defs().into_iter().cloned().collect())
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
fn test_parse_fstring_literal_folds_to_string_lit() {
    let source = r#"
        fn test() -> string {
            f"hello {{}}"
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (_, tail) = block_parts(&func.body);
    let tail = tail.expect("Expected block tail");

    match &tail.kind {
        ExprKind::StringLit { value, .. } => {
            assert_eq!(value, "hello {}");
        }
        _ => panic!("Expected folded StringLit"),
    }
}

#[test]
fn test_parse_fstring_string_lit_expr_folds() {
    let source = r#"
        fn test() -> string {
            f"{\"hi\"}"
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (_, tail) = block_parts(&func.body);
    let tail = tail.expect("Expected block tail");

    match &tail.kind {
        ExprKind::StringLit { value, .. } => {
            assert_eq!(value, "hi");
        }
        _ => panic!("Expected folded StringLit"),
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
    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Array { elem_ty_expr, dims } => {
            match &elem_ty_expr.kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            assert_eq!(dims.as_slice(), &[2, 3]);
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

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Array { elem_ty_expr, dims } => {
            match &elem_ty_expr.kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            assert_eq!(dims.as_slice(), &[2, 3, 4]);
        }
        _ => panic!("Expected array type"),
    }
}

#[test]
fn test_parse_dyn_array_type() {
    let source = r#"
        fn test() -> u64[*] {
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::DynArray { elem_ty_expr } => match &elem_ty_expr.kind {
            TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
            _ => panic!("Expected named element type"),
        },
        _ => panic!("Expected dyn-array type"),
    }
}

#[test]
fn test_parse_refined_type_multiple_refinements() {
    let source = r#"
        type NonZeroSmall = u64: bounds(0, 10) & nonzero;
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_defs = module.type_defs();
    assert_eq!(type_defs.len(), 1);

    let type_def = type_defs[0];
    match &type_def.kind {
        TypeDefKind::Alias { aliased_ty } => match &aliased_ty.kind {
            TypeExprKind::Refined { refinements, .. } => {
                assert_eq!(refinements.len(), 2);
                match (&refinements[0], &refinements[1]) {
                    (RefinementKind::Bounds { min, max }, RefinementKind::NonZero) => {
                        assert_eq!(*min, 0);
                        assert_eq!(*max, 10);
                    }
                    _ => panic!("Expected bounds & nonzero refinements"),
                }
            }
            _ => panic!("Expected refined type expression"),
        },
        _ => panic!("Expected type alias"),
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

    let (items, block_tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let block_tail = block_tail.expect("Expected block to have a tail expr");
    if let ExprKind::ArrayIndex { target, indices } = &block_tail.kind {
        // Check target is Var
        match &target.kind {
            ExprKind::Var { ident: name, .. } => assert_eq!(name, "arr"),
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
        if let ExprKind::ArrayLit {
            elem_ty: None,
            init: ArrayLitInit::Elems(outer_elems),
        } = &value.kind
        {
            assert_eq!(outer_elems.len(), 2);

            // Each element should be an ArrayLit
            for elem in outer_elems {
                match &elem.kind {
                    ExprKind::ArrayLit {
                        elem_ty: None,
                        init: ArrayLitInit::Elems(inner_elems),
                    } => {
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
fn test_parse_heap_type_and_alloc_expr() {
    let source = r#"
        fn test() -> u64^ {
            ^1
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Heap { elem_ty_expr } => match &elem_ty_expr.kind {
            TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
            _ => panic!("Expected named element type"),
        },
        _ => panic!("Expected heap type"),
    }

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::HeapAlloc { expr } => match &expr.kind {
            ExprKind::IntLit(value) => assert_eq!(*value, 1),
            _ => panic!("Expected int literal"),
        },
        _ => panic!("Expected heap alloc expression"),
    }
}

#[test]
fn test_parse_heap_type_postfix_with_array_precedence() {
    let source = r#"
        fn a(x: u64^[3]) -> u64 { 0 }
        fn b(x: u64[3]^ ) -> u64 { 0 }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    assert_eq!(funcs.len(), 2);

    // u64^[3] => Array(Heap(u64), [3])
    let a_param = &funcs[0].sig.params[0];
    match &a_param.typ.kind {
        TypeExprKind::Array { elem_ty_expr, dims } => {
            assert_eq!(dims, &[3]);
            match &elem_ty_expr.kind {
                TypeExprKind::Heap { elem_ty_expr } => match &elem_ty_expr.kind {
                    TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                    _ => panic!("Expected named u64 inside heap"),
                },
                _ => panic!("Expected heap element type"),
            }
        }
        _ => panic!("Expected array of heap element type"),
    }

    // u64[3]^ => Heap(Array(u64, [3]))
    let b_param = &funcs[1].sig.params[0];
    match &b_param.typ.kind {
        TypeExprKind::Heap { elem_ty_expr } => match &elem_ty_expr.kind {
            TypeExprKind::Array { elem_ty_expr, dims } => {
                assert_eq!(dims, &[3]);
                match &elem_ty_expr.kind {
                    TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                    _ => panic!("Expected named u64 inside array"),
                }
            }
            _ => panic!("Expected array type inside heap"),
        },
        _ => panic!("Expected heap type"),
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
        if let ExprKind::ArrayLit {
            elem_ty: Some(elem_ty),
            init: ArrayLitInit::Elems(elems),
        } = &value.kind
        {
            match &elem_ty.kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u8"),
                _ => panic!("Expected named type"),
            }
            assert_eq!(elems.len(), 3);
            for elem in elems {
                assert!(matches!(elem.kind, ExprKind::IntLit(_)));
            }
        } else {
            panic!("Expected typed ArrayLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_array_repeat_literal() {
    let source = r#"
        fn test() -> u64 {
            let arr = u8[0; 4];
            arr[0]
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::ArrayLit {
            elem_ty: Some(elem_ty),
            init: ArrayLitInit::Repeat(expr, count),
        } = &value.kind
        {
            match &elem_ty.kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u8"),
                _ => panic!("Expected named type"),
            }
            assert_eq!(*count, 4);
            assert!(matches!(expr.kind, ExprKind::IntLit(_)));
        } else {
            panic!("Expected typed repeat ArrayLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_set_literal() {
    let source = r#"
        fn test() -> u64 {
            let s = {1, 2, 3};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::SetLit { elem_ty, elems } = &value.kind {
            assert!(elem_ty.is_none());
            assert_eq!(elems.len(), 3);
            for elem in elems {
                assert!(matches!(elem.kind, ExprKind::IntLit(_)));
            }
        } else {
            panic!("Expected SetLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_typed_empty_set_literal() {
    let source = r#"
        fn test() -> u64 {
            let s = set<u64>{};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::SetLit {
            elem_ty: Some(elem_ty),
            elems,
        } = &value.kind
        {
            assert!(elems.is_empty());
            match &elem_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                _ => panic!("Expected named u64 element type"),
            }
        } else {
            panic!("Expected typed SetLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_singleton_bare_braces_remains_block_expr() {
    let source = r#"
        fn test() -> u64 {
            let s = {1};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        assert!(matches!(value.kind, ExprKind::Block { .. }));
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_typed_singleton_set_allows_no_trailing_comma() {
    let source = r#"
        fn test() -> u64 {
            let s = set<u64>{1};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::SetLit {
            elem_ty: Some(elem_ty),
            elems,
        } = &value.kind
        {
            assert_eq!(elems.len(), 1);
            assert!(matches!(elems[0].kind, ExprKind::IntLit(1)));
            match &elem_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                _ => panic!("Expected named u64 element type"),
            }
        } else {
            panic!("Expected typed SetLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_map_literal() {
    let source = r#"
        fn test() -> u64 {
            let m = {"one": 1, "two": 2};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::MapLit {
            key_ty,
            value_ty,
            entries,
        } = &value.kind
        {
            assert!(key_ty.is_none());
            assert!(value_ty.is_none());
            assert_eq!(entries.len(), 2);
            assert!(matches!(entries[0].key.kind, ExprKind::StringLit { .. }));
            assert!(matches!(entries[0].value.kind, ExprKind::IntLit(1)));
            assert!(matches!(entries[1].key.kind, ExprKind::StringLit { .. }));
            assert!(matches!(entries[1].value.kind, ExprKind::IntLit(2)));
        } else {
            panic!("Expected MapLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_typed_empty_map_literal() {
    let source = r#"
        fn test() -> u64 {
            let m = map<string, u64>{};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::MapLit {
            key_ty: Some(key_ty),
            value_ty: Some(value_ty),
            entries,
        } = &value.kind
        {
            assert!(entries.is_empty());
            match &key_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "string"),
                _ => panic!("Expected named string key type"),
            }
            match &value_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                _ => panic!("Expected named u64 value type"),
            }
        } else {
            panic!("Expected typed MapLit");
        }
    } else {
        panic!("Expected Let");
    }
}

#[test]
fn test_parse_typed_singleton_map_allows_no_trailing_comma() {
    let source = r#"
        fn test() -> u64 {
            let m = map<u64, bool>{1: true};
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    if let StmtExprKind::LetBind { value, .. } = &stmt.kind {
        if let ExprKind::MapLit {
            key_ty: Some(key_ty),
            value_ty: Some(value_ty),
            entries,
        } = &value.kind
        {
            assert_eq!(entries.len(), 1);
            assert!(matches!(entries[0].key.kind, ExprKind::IntLit(1)));
            assert!(matches!(entries[0].value.kind, ExprKind::BoolLit(true)));
            match &key_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
                _ => panic!("Expected named u64 key type"),
            }
            match &value_ty.kind {
                TypeExprKind::Named { ident, .. } => assert_eq!(ident, "bool"),
                _ => panic!("Expected named bool value type"),
            }
        } else {
            panic!("Expected typed MapLit");
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
    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Tuple { field_ty_exprs } => {
            assert_eq!(field_ty_exprs.len(), 2);
            match &field_ty_exprs[0].kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            match &field_ty_exprs[1].kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "bool"),
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

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Tuple { field_ty_exprs } => {
            assert_eq!(field_ty_exprs.len(), 2);
            match &field_ty_exprs[0].kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named type"),
            }
            match &field_ty_exprs[1].kind {
                TypeExprKind::Tuple {
                    field_ty_exprs: inner_ty_exprs,
                } => {
                    assert_eq!(inner_ty_exprs.len(), 2);
                    match &inner_ty_exprs[0].kind {
                        TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "bool"),
                        _ => panic!("Expected named type"),
                    }
                    match &inner_ty_exprs[1].kind {
                        TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
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
    let type_defs = module.type_defs();
    assert_eq!(type_defs.len(), 1);

    let type_def = type_defs[0];
    assert_eq!(type_def.name, "Color");
    match &type_def.kind {
        TypeDefKind::Enum { variants } => {
            let names = variants.iter().map(|v| v.name.as_str()).collect::<Vec<_>>();
            assert_eq!(names, vec!["Red", "Green", "Blue"]);
        }
        _ => panic!("Expected enum type def"),
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
            ..
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
    let type_defs = module.type_defs();
    assert_eq!(type_defs.len(), 1);

    let type_def = type_defs[0];
    assert_eq!(type_def.name, "Option");
    match &type_def.kind {
        TypeDefKind::Enum { variants } => {
            assert_eq!(variants.len(), 2);
            assert_eq!(variants[0].name, "None");
            assert!(variants[0].payload.is_empty());
            assert_eq!(variants[1].name, "Some");
            assert_eq!(variants[1].payload.len(), 1);
            match &variants[1].payload[0].kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named payload type"),
            }
        }
        _ => panic!("Expected enum type def"),
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
            ..
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
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "p"),
                _ => panic!("Expected StructUpdate target to be Var"),
            }
            assert_eq!(fields.len(), 1);
            assert_eq!(fields[0].name, "color");
            match &fields[0].value.kind {
                ExprKind::EnumVariant {
                    enum_name,
                    variant,
                    payload,
                    ..
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
fn test_parse_string_literal() {
    let source = r#"
        fn main() {
            "café"
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::StringLit { value } => {
            assert_eq!(value, "café");
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
            ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
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
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
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
fn test_parse_method_call() {
    let source = r#"
        fn test() -> u64 {
            let p = 1;
            p.sum(2, 3)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 1);

    let tail = tail.expect("Expected block to have a tail expr");
    if let ExprKind::MethodCall {
        callee,
        method_name,
        args,
    } = &tail.kind
    {
        match &callee.kind {
            ExprKind::Var { ident: name, .. } => assert_eq!(name, "p"),
            _ => panic!("Expected Var target"),
        }
        assert_eq!(method_name, "sum");
        assert_eq!(args.len(), 2);
        assert!(matches!(args[0].expr.kind, ExprKind::IntLit(2)));
        assert!(matches!(args[1].expr.kind, ExprKind::IntLit(3)));
    } else {
        panic!("Expected MethodCall");
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
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
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
            BindPatternKind::Tuple { patterns } => {
                assert_eq!(patterns.len(), 2);

                // First pattern should be identifier "a"
                match &patterns[0].kind {
                    BindPatternKind::Name { ident, .. } => assert_eq!(ident, "a"),
                    _ => panic!("Expected Ident pattern"),
                }

                // Second pattern should be identifier "b"
                match &patterns[1].kind {
                    BindPatternKind::Name { ident, .. } => assert_eq!(ident, "b"),
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
            BindPatternKind::Tuple { patterns } => {
                assert_eq!(patterns.len(), 2);

                // First pattern should be identifier "a"
                match &patterns[0].kind {
                    BindPatternKind::Name { ident, .. } => assert_eq!(ident, "a"),
                    _ => panic!("Expected Ident pattern"),
                }

                // Second pattern should be a nested tuple pattern
                match &patterns[1].kind {
                    BindPatternKind::Tuple { patterns: inner } => {
                        assert_eq!(inner.len(), 2);
                        match &inner[0].kind {
                            BindPatternKind::Name { ident, .. } => assert_eq!(ident, "b"),
                            _ => panic!("Expected Ident pattern"),
                        }
                        match &inner[1].kind {
                            BindPatternKind::Name { ident, .. } => assert_eq!(ident, "c"),
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
            BindPatternKind::Tuple { patterns } => {
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
            BindPatternKind::Name { ident, .. } => assert_eq!(ident, "a"),
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
                Red(_) => 0,
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
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "c"),
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
                    assert!(matches!(&bindings[0], MatchPatternBinding::Wildcard { .. }));
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
                    assert!(matches!(
                        &bindings[0],
                        MatchPatternBinding::Named { ident, .. } if ident == "y"
                    ));
                }
                _ => panic!("Expected enum variant pattern in arm 1"),
            }

            assert!(matches!(arms[2].pattern, MatchPattern::Wildcard { .. }));
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_enum_variant_pattern_with_type_args() {
    let source = r#"
        type Option<T> = None | Some(T)

        fn test(opt: Option<u64>) -> u64 {
            match opt {
                Option<u64>::Some(x) => x,
                Option<u64>::None => 0,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    let ExprKind::Match { arms, .. } = &tail.kind else {
        panic!("Expected match expression");
    };

    let MatchPattern::EnumVariant {
        enum_name,
        type_args,
        variant_name,
        ..
    } = &arms[0].pattern
    else {
        panic!("Expected enum variant pattern in arm 0");
    };

    assert_eq!(enum_name.as_deref(), Some("Option"));
    assert_eq!(variant_name, "Some");
    assert_eq!(type_args.len(), 1);
    match &type_args[0].kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "u64"),
        _ => panic!("Expected named type arg"),
    }
}

#[test]
fn test_parse_match_expr_bool_patterns() {
    let source = r#"
        fn test(b: bool) -> u64 {
            match b {
                true => 1,
                false => 0,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::Match { scrutinee, arms } => {
            match &scrutinee.kind {
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "b"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 2);

            match &arms[0].pattern {
                MatchPattern::BoolLit { value, .. } => assert!(*value),
                _ => panic!("Expected bool literal pattern in arm 0"),
            }

            match &arms[1].pattern {
                MatchPattern::BoolLit { value, .. } => assert!(!*value),
                _ => panic!("Expected bool literal pattern in arm 1"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_int_patterns() {
    let source = r#"
        fn test(x: u64) -> u64 {
            match x {
                0 => 1,
                42 => 2,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::Match { scrutinee, arms } => {
            match &scrutinee.kind {
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "x"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 2);

            match &arms[0].pattern {
                MatchPattern::IntLit { value, .. } => assert_eq!(*value, 0),
                _ => panic!("Expected int literal pattern in arm 0"),
            }

            match &arms[1].pattern {
                MatchPattern::IntLit { value, .. } => assert_eq!(*value, 42),
                _ => panic!("Expected int literal pattern in arm 1"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_tuple_patterns() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (x, _) => x,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::Match { scrutinee, arms } => {
            match &scrutinee.kind {
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 1);

            match &arms[0].pattern {
                MatchPattern::Tuple { patterns, .. } => {
                    assert_eq!(patterns.len(), 2);
                    assert!(
                        matches!(&patterns[0], MatchPattern::Binding { ident, .. } if ident == "x")
                    );
                    assert!(matches!(&patterns[1], MatchPattern::Wildcard { .. }));
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_tuple_patterns_nested() {
    let source = r#"
        fn test(t: (u64, (bool, u64))) -> u64 {
            match t {
                (x, (y, _)) => x,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::Match { scrutinee, arms } => {
            match &scrutinee.kind {
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 1);

            match &arms[0].pattern {
                MatchPattern::Tuple { patterns, .. } => {
                    assert_eq!(patterns.len(), 2);
                    assert!(matches!(
                        &patterns[0],
                        MatchPattern::Binding { ident, .. } if ident == "x"
                    ));
                    match &patterns[1] {
                        MatchPattern::Tuple { patterns, .. } => {
                            assert_eq!(patterns.len(), 2);
                            assert!(matches!(
                                &patterns[0],
                                MatchPattern::Binding { ident, .. } if ident == "y"
                            ));
                            assert!(matches!(&patterns[1], MatchPattern::Wildcard { .. }));
                        }
                        _ => panic!("Expected nested tuple pattern"),
                    }
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_tuple_patterns_literals() {
    let source = r#"
        type Flag = On | Off

        fn test(t: (u64, Flag, bool)) -> u64 {
            match t {
                (1, Flag::On, true) => 1,
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
                ExprKind::Var { ident: name, .. } => assert_eq!(name, "t"),
                _ => panic!("Expected scrutinee Var"),
            }
            assert_eq!(arms.len(), 2);

            match &arms[0].pattern {
                MatchPattern::Tuple { patterns, .. } => {
                    assert_eq!(patterns.len(), 3);
                    assert!(
                        matches!(&patterns[0], MatchPattern::IntLit { value, .. } if *value == 1)
                    );
                    assert!(matches!(
                        &patterns[1],
                        MatchPattern::EnumVariant { variant_name, .. } if variant_name == "On"
                    ));
                    assert!(matches!(&patterns[2], MatchPattern::BoolLit { value, .. } if *value));
                }
                _ => panic!("Expected tuple pattern"),
            }
            assert!(matches!(arms[1].pattern, MatchPattern::Wildcard { .. }));
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_typed_binding_pattern() {
    let source = r#"
        fn test(t: (u64, bool)) -> u64 {
            match t {
                (x: u64, _) => x,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Match { arms, .. } => {
            assert_eq!(arms.len(), 1);
            match &arms[0].pattern {
                MatchPattern::Tuple { patterns, .. } => {
                    assert_eq!(patterns.len(), 2);
                    match &patterns[0] {
                        MatchPattern::TypedBinding { ident, ty_expr, .. } => {
                            assert_eq!(ident, "x");
                            assert!(matches!(
                                &ty_expr.kind,
                                TypeExprKind::Named { ident, .. } if ident == "u64"
                            ));
                        }
                        _ => panic!("Expected typed binding pattern"),
                    }
                    assert!(matches!(&patterns[1], MatchPattern::Wildcard { .. }));
                }
                _ => panic!("Expected tuple pattern"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_return_error_union_type() {
    let source = r#"
        type Config = { host: string }
        type IoError = { code: u64 }
        type ParseError = { line: u64 }

        fn load() -> Config | IoError | ParseError {
            Config { host: "localhost" }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Union { variants } => {
            assert_eq!(variants.len(), 3);
            assert!(matches!(
                &variants[0].kind,
                TypeExprKind::Named { ident, .. } if ident == "Config"
            ));
            assert!(matches!(
                &variants[1].kind,
                TypeExprKind::Named { ident, .. } if ident == "IoError"
            ));
            assert!(matches!(
                &variants[2].kind,
                TypeExprKind::Named { ident, .. } if ident == "ParseError"
            ));
        }
        other => panic!("Expected union return type, got {other:?}"),
    }
}

#[test]
fn test_parse_postfix_try_operator() {
    let source = r#"
        type IoError = { code: u64 }

        fn read() -> u64 | IoError {
            1
        }

        fn test() -> u64 | IoError {
            read()?
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let test_fn = funcs
        .iter()
        .find(|func| func.sig.name == "test")
        .expect("Missing test function");

    let tail = block_tail(&test_fn.body);
    match &tail.kind {
        ExprKind::UnaryOp { op, expr } => {
            assert!(matches!(op, UnaryOp::Try));
            assert!(matches!(&expr.kind, ExprKind::Call { .. }));
        }
        other => panic!("Expected unary try expression, got {other:?}"),
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
            BindPatternKind::Name { ident, .. } => assert_eq!(ident, "i"),
            _ => panic!("Expected ident pattern"),
        }
        match &iter.kind {
            ExprKind::Range { start, end } => {
                assert!(matches!(start.kind, ExprKind::IntLit(0)));
                assert!(matches!(end.kind, ExprKind::IntLit(3)));
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
fn test_parse_for_range_expr_bounds() {
    let source = r#"
        fn test() -> u64 {
            let start = 0;
            let end = 3;
            for i in start..end { i; }
            0
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _tail) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 2);

    if let StmtExprKind::For { iter, .. } = &stmt.kind {
        match &iter.kind {
            ExprKind::Range { start, end } => {
                assert!(matches!(start.kind, ExprKind::Var { .. }));
                assert!(matches!(end.kind, ExprKind::Var { .. }));
            }
            _ => panic!("Expected range iterator"),
        }
    } else {
        panic!("Expected for loop");
    }
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
            BindPatternKind::Name { ident, .. } => assert_eq!(ident, "x"),
            _ => panic!("Expected ident pattern"),
        }
        assert!(matches!(iter.kind, ExprKind::ArrayLit { .. }));
        assert!(matches!(body.kind, ExprKind::Block { .. }));
    } else {
        panic!("Expected for loop");
    }

    let tail = tail.expect("Expected block to have a tail expr");
    assert!(matches!(tail.kind, ExprKind::IntLit(0)));
}

#[test]
fn test_parse_if_optional_else_block() {
    let source = r#"
        fn test() -> () {
            if true { };
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, tail) = block_parts(&func.body);
    let item = match &items[0] {
        BlockItem::Expr(expr) => expr,
        _ => panic!("Expected expr block item"),
    };

    let ExprKind::If {
        then_body,
        else_body,
        ..
    } = &item.kind
    else {
        panic!("Expected if expression");
    };

    let ExprKind::Block {
        items: then_items,
        tail: then_tail,
    } = &then_body.kind
    else {
        panic!("Expected block then body");
    };
    assert!(then_items.is_empty());
    assert!(then_tail.is_none());

    let ExprKind::Block {
        items: else_items,
        tail: else_tail,
    } = &else_body.kind
    else {
        panic!("Expected block else body");
    };
    assert!(else_items.is_empty());
    assert!(else_tail.is_none());

    let tail = tail.expect("Expected block to have a tail expr");
    assert!(matches!(tail.kind, ExprKind::UnitLit));
}

#[test]
fn test_parse_else_if_wraps_in_block() {
    let source = r#"
        fn test() -> () {
            if true { } else if false { } else { };
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let (items, _) = block_parts(&func.body);
    let item = match &items[0] {
        BlockItem::Expr(expr) => expr,
        _ => panic!("Expected expr block item"),
    };

    let ExprKind::If { else_body, .. } = &item.kind else {
        panic!("Expected if expression");
    };

    let ExprKind::Block { tail, .. } = &else_body.kind else {
        panic!("Expected block else body");
    };
    let tail = tail.as_deref().expect("Expected nested if tail");
    assert!(matches!(tail.kind, ExprKind::If { .. }));
}

#[test]
fn test_parse_closure_expr() {
    let source = r#"
        fn test() -> u64 {
            |a: u64, b: u64| -> u64 a + b
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Closure {
            ident,
            captures,
            params,
            return_ty,
            body,
            ..
        } => {
            assert_eq!(ident, "test$closure$1");
            assert!(captures.is_empty());
            assert_eq!(params.len(), 2);
            assert_eq!(params[0].ident, "a");
            assert_eq!(params[1].ident, "b");
            match &return_ty.kind {
                TypeExprKind::Named { ident: name, .. } => assert_eq!(name, "u64"),
                _ => panic!("Expected named return type"),
            }
            match &body.kind {
                ExprKind::Block { tail, .. } => {
                    let tail = tail.as_ref().expect("Expected block tail");
                    assert!(matches!(tail.kind, ExprKind::BinOp { .. }));
                }
                _ => panic!("Expected closure body block"),
            }
        }
        _ => panic!("Expected closure expression"),
    }
}

#[test]
fn test_parse_closure_expr_empty_params() {
    let source = r#"
        fn test() -> u64 {
            || { 1 }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Closure {
            ident,
            captures,
            params,
            return_ty,
            body,
            ..
        } => {
            assert_eq!(ident, "test$closure$1");
            assert!(captures.is_empty());
            assert!(params.is_empty());
            match &return_ty.kind {
                TypeExprKind::Infer => {}
                _ => panic!("Expected inferred return type"),
            }
            assert!(matches!(body.kind, ExprKind::Block { .. }));
        }
        _ => panic!("Expected closure expression"),
    }
}

#[test]
fn test_parse_closure_capture_list() {
    let source = r#"
        fn test() -> u64 {
            [move x, y] || x + y
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Closure { captures, .. } => {
            assert_eq!(captures.len(), 2);
            let CaptureSpec::Move { ident, .. } = &captures[0];
            assert_eq!(ident, "x");
            let CaptureSpec::Move { ident, .. } = &captures[1];
            assert_eq!(ident, "y");
        }
        _ => panic!("Expected closure expression"),
    }
}

#[test]
fn test_parse_intrinsic_func_attr() {
    let source = "@intrinsic fn foo() { 1 }";
    let funcs = parse_source(source).expect("Failed to parse");

    assert_eq!(funcs.len(), 1);
    assert_eq!(funcs[0].attrs.len(), 1);
    assert_eq!(funcs[0].attrs[0].name, "intrinsic");
    assert!(funcs[0].attrs[0].args.is_empty());
}

#[test]
fn test_parse_intrinsic_method_attr() {
    let source = r#"
        type Foo = {}
        Foo :: {
            @intrinsic fn bar(self) { 1 }
        }
    "#;
    let module = parse_module(source).expect("Failed to parse");
    let method_blocks = module.method_blocks();

    assert_eq!(method_blocks.len(), 1);
    assert_eq!(method_blocks[0].method_items.len(), 1);
    match &method_blocks[0].method_items[0] {
        MethodItem::Def(method_def) => {
            assert_eq!(method_def.attrs.len(), 1);
            assert_eq!(method_def.attrs[0].name, "intrinsic");
            assert!(method_def.attrs[0].args.is_empty());
        }
        MethodItem::Decl(_) => panic!("Expected method definition"),
    }
}

#[test]
fn test_parse_link_name_attr() {
    let source = "@link_name(\"__mc_foo\") fn foo() { 1 }";
    let funcs = parse_source(source).expect("Failed to parse");

    assert_eq!(funcs.len(), 1);
    assert_eq!(funcs[0].attrs.len(), 1);
    assert_eq!(funcs[0].attrs[0].name, "link_name");
    assert_eq!(funcs[0].attrs[0].args.len(), 1);
    match &funcs[0].attrs[0].args[0] {
        AttrArg::String(value) => assert_eq!(value, "__mc_foo"),
    }
}

#[test]
fn test_parse_method_decl() {
    let source = r#"
        type Foo = {}
        Foo :: {
            @intrinsic fn len(self) -> u64;
        }
    "#;
    let module = parse_module(source).expect("Failed to parse");
    let method_blocks = module.method_blocks();

    assert_eq!(method_blocks.len(), 1);
    assert_eq!(method_blocks[0].method_items.len(), 1);
    match &method_blocks[0].method_items[0] {
        MethodItem::Decl(method_decl) => {
            assert_eq!(method_decl.sig.name, "len");
        }
        MethodItem::Def(_) => panic!("Expected method declaration"),
    }
}

#[test]
fn test_parse_attr_on_method_block_rejected() {
    let source = "@intrinsic Foo :: { fn bar(self) { 1 } }";
    let result = parse_module(source);

    assert!(matches!(result, Err(ParseError::AttributeNotAllowed(_))));
}

#[test]
fn test_parse_trait_def_and_trait_method_block() {
    let source = r#"
        trait Runnable {
            fn run(self);
        }

        type Process = { name: string }

        Process :: Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let trait_defs = module.trait_defs();
    assert_eq!(trait_defs.len(), 1);
    assert_eq!(trait_defs[0].name, "Runnable");
    assert_eq!(trait_defs[0].methods.len(), 1);
    assert_eq!(trait_defs[0].methods[0].sig.name, "run");

    let method_blocks = module.method_blocks();
    assert_eq!(method_blocks.len(), 1);
    assert_eq!(method_blocks[0].type_name, "Process");
    assert_eq!(method_blocks[0].trait_name.as_deref(), Some("Runnable"));
}

#[test]
fn test_parse_trait_method_block_with_module_qualified_trait_name() {
    let source = r#"
        type Process = { name: string }

        Process :: rt::Runnable {
            fn run(self) {
                ()
            }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let method_blocks = module.method_blocks();
    assert_eq!(method_blocks.len(), 1);
    assert_eq!(method_blocks[0].type_name, "Process");
    assert_eq!(method_blocks[0].trait_name.as_deref(), Some("rt::Runnable"));
}

#[test]
fn test_parse_type_param_with_trait_bound() {
    let source = r#"
        trait Runnable {
            fn run(self);
        }

        fn execute<T: Runnable>(value: T) {
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let execute = funcs
        .iter()
        .find(|func| func.sig.name == "execute")
        .expect("missing execute");
    assert_eq!(execute.sig.type_params.len(), 1);
    let bound = execute.sig.type_params[0]
        .bound
        .as_ref()
        .expect("missing trait bound");
    assert_eq!(bound.name, "Runnable");
}

#[test]
fn test_parse_type_param_with_module_qualified_trait_bound() {
    let source = r#"
        fn execute<T: io::Runnable>(value: T) {
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let execute = funcs
        .iter()
        .find(|func| func.sig.name == "execute")
        .expect("missing execute");
    assert_eq!(execute.sig.type_params.len(), 1);
    let bound = execute.sig.type_params[0]
        .bound
        .as_ref()
        .expect("missing trait bound");
    assert_eq!(bound.name, "io::Runnable");
}

#[test]
fn test_parse_trait_property_contracts() {
    let source = r#"
        trait HasLength {
            prop len: u64 { get; }
            prop value: u64 { set; }
            prop count: u64 { get; set; }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let trait_defs = module.trait_defs();
    assert_eq!(trait_defs.len(), 1);
    let trait_def = trait_defs[0];
    assert_eq!(trait_def.properties.len(), 3);

    let len = trait_def
        .properties
        .iter()
        .find(|p| p.name == "len")
        .expect("missing len property");
    assert!(len.has_get);
    assert!(!len.has_set);

    let value = trait_def
        .properties
        .iter()
        .find(|p| p.name == "value")
        .expect("missing value property");
    assert!(!value.has_get);
    assert!(value.has_set);

    let count = trait_def
        .properties
        .iter()
        .find(|p| p.name == "count")
        .expect("missing count property");
    assert!(count.has_get);
    assert!(count.has_set);
}

#[test]
fn test_parse_requires_block_with_default_alias() {
    let source = r#"
        requires {
            std::io
            app::config::loader
        }

        fn main() -> u64 { 0 }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    assert_eq!(module.requires.len(), 2);

    assert_eq!(module.requires[0].path, vec!["std", "io"]);
    assert_eq!(module.requires[0].alias, None);

    assert_eq!(module.requires[1].path, vec!["app", "config", "loader"]);
    assert_eq!(module.requires[1].alias, None);
}

#[test]
fn test_parse_requires_block_with_explicit_alias() {
    let source = r#"
        requires {
            std::parse as parse
            std::parse::u64 as parse_u64
        }

        fn main() -> u64 { 0 }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    assert_eq!(module.requires.len(), 2);

    assert_eq!(module.requires[0].path, vec!["std", "parse"]);
    assert_eq!(module.requires[0].alias.as_deref(), Some("parse"));

    assert_eq!(module.requires[1].path, vec!["std", "parse", "u64"]);
    assert_eq!(module.requires[1].alias.as_deref(), Some("parse_u64"));
}

#[test]
fn test_parse_module_qualified_type_reference() {
    let source = r#"
        fn use_config(cfg: app::Config) -> app::Config {
            cfg
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = funcs.first().expect("missing function");

    match &func.sig.params[0].typ.kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "app::Config"),
        _ => panic!("expected named type"),
    }

    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "app::Config"),
        _ => panic!("expected named return type"),
    }
}

#[test]
fn test_parse_requires_rejects_dot_separator() {
    let source = r#"
        requires {
            std.io
        }

        fn main() -> u64 { 0 }
    "#;

    assert!(parse_module(source).is_err());
}

#[test]
fn test_parse_named_type_rejects_dot_separator() {
    let source = r#"
        fn use_config(cfg: app.Config) -> u64 {
            0
        }
    "#;

    assert!(parse_source(source).is_err());
}

#[test]
fn test_parse_typestate_disabled_by_default() {
    let source = r#"
        typestate Connection {
            fields {
                addr: string,
            }

            fn new(addr: string) -> Disconnected {
                Disconnected { addr: addr }
            }

            state Disconnected {
                fn connect(fd: u64) -> Connected {
                    Connected { fd: fd }
                }
            }

            state Connected {
                fields {
                    fd: u64,
                }
            }
        }
    "#;

    let err = parse_module(source).expect_err("typestate should require experimental flag");
    assert!(matches!(
        err,
        ParseError::FeatureDisabled {
            feature: "typestate",
            ..
        }
    ));
}

#[test]
fn test_parse_typestate_with_experimental_flag() {
    let source = r#"
        typestate Connection {
            fields {
                addr: string,
            }

            fn new(addr: string) -> Disconnected {
                Disconnected { addr: addr }
            }

            state Disconnected {
                fn connect(fd: u64) -> Connected {
                    Connected { fd: fd }
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("typestate should parse when experimental flag is enabled");

    assert_eq!(module.top_level_items.len(), 1);
    let typestate = match &module.top_level_items[0] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };

    assert_eq!(typestate.name, "Connection");
    assert_eq!(typestate.items.len(), 3);
}

#[test]
fn test_parse_protocol_transition_syntax_with_experimental_flag() {
    let source = r#"
        protocol Auth {
            msg Start;
            msg AuthorizeReq;
            msg AuthApproved;
            msg AuthDenied;
            msg SessionRevoked;

            req Client -> Server: AuthorizeReq => AuthApproved | AuthDenied;

            role Client {
                state Idle {
                    on Start -> Awaiting {
                        effects: [ AuthorizeReq ~> Server ]
                    }
                }

                state Awaiting {
                    on AuthApproved@Server -> Idle;
                    on AuthDenied@Server -> Idle;
                }
            }

            role Server {
                state Ready {
                    on AuthorizeReq@Client -> Ready {
                        effects: [ SessionRevoked ~> Client ]
                    }
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("protocol should parse when experimental flag is enabled");

    assert_eq!(module.top_level_items.len(), 1);
    let protocol = match &module.top_level_items[0] {
        TopLevelItem::ProtocolDef(def) => def,
        _ => panic!("expected protocol top-level item"),
    };

    assert_eq!(protocol.name, "Auth");
    assert_eq!(protocol.messages.len(), 5);
    assert_eq!(protocol.request_contracts.len(), 1);
    assert_eq!(protocol.roles.len(), 2);
    assert_eq!(protocol.roles[0].name, "Client");
    assert_eq!(protocol.roles[1].name, "Server");
    assert_eq!(protocol.roles[0].states.len(), 2);
    assert_eq!(protocol.roles[1].states.len(), 1);
    assert_eq!(protocol.roles[0].states[0].transitions.len(), 1);
    assert_eq!(protocol.roles[0].states[1].transitions.len(), 2);
    assert_eq!(protocol.roles[1].states[0].transitions.len(), 1);
    assert_eq!(protocol.flows.len(), 6);
    assert_eq!(protocol.flows[0].from_role, "Client");
    assert_eq!(protocol.flows[0].to_role, "Server");
    assert_eq!(protocol.flows[0].response_tys.len(), 2);
    assert_eq!(protocol.flows[1].response_tys.len(), 0);
}

#[test]
fn test_parse_protocol_non_start_trigger_requires_explicit_source_role() {
    let source = r#"
        protocol Auth {
            role Client {
                state Idle {
                    on Ping -> Idle;
                }
            }
        }
    "#;

    let err = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect_err("non-start trigger without source role should fail");

    assert!(matches!(err, ParseError::ExpectedToken(TK::At, _)));
}

#[test]
fn test_parse_typestate_role_impl_and_on_handlers() {
    let source = r#"
        typestate Gateway : Auth::Client {
            fn new() -> Ready {
                Ready
            }

            on Tick(t: Tick) -> Ready {
                Ready
            }

            state Ready {
                on Incoming(msg: Incoming, cap: ReplyCap<Ack | Nack>) -> Ready {
                    let pending = emit Request(to: self.peer, Outgoing {});
                    emit Send(to: self.peer, Outgoing {});
                    reply(cap, Ack {});
                    Ready
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("typestate with role impl/on handlers should parse");

    let typestate = match &module.top_level_items[0] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };

    assert_eq!(typestate.role_impls.len(), 1);
    assert_eq!(typestate.role_impls[0].path, vec!["Auth", "Client"]);

    let top_handler = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::Handler(handler) => Some(handler),
            _ => None,
        })
        .expect("expected typestate-level on handler");

    let state = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::State(state) => Some(state),
            _ => None,
        })
        .expect("expected state item");

    let state_handler = state
        .items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Handler(handler) => Some(handler),
            _ => None,
        })
        .expect("expected state on handler");

    assert!(matches!(
        top_handler.selector_ty.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "Tick"
    ));
    assert_eq!(state_handler.params.len(), 2);

    let ExprKind::Block { items, .. } = &state_handler.body.kind else {
        panic!("expected block body for state handler");
    };

    assert!(
        items.iter().any(|item| match item {
            BlockItem::Stmt(StmtExpr {
                kind: StmtExprKind::LetBind { value, .. },
                ..
            }) => matches!(
                value.kind,
                ExprKind::Emit {
                    kind: EmitKind::Request { .. }
                }
            ),
            _ => false,
        }),
        "expected emit request in let-binding value"
    );
    assert!(
        items.iter().any(|item| match item {
            BlockItem::Expr(expr) => matches!(
                expr.kind,
                ExprKind::Emit {
                    kind: EmitKind::Send { .. }
                }
            ),
            _ => false,
        }),
        "expected emit send expression statement in state handler body"
    );
    assert!(
        items.iter().any(|item| match item {
            BlockItem::Expr(expr) => matches!(expr.kind, ExprKind::Reply { .. }),
            _ => false,
        }),
        "expected reply expression statement in state handler body"
    );
}

#[test]
fn test_parse_typestate_pattern_on_handler_desugars_to_canonical_params() {
    let source = r#"
        typestate Gateway {
            fn new() -> AwaitAuth { AwaitAuth }

            state AwaitAuth {
                on Response(pending, AuthApproved) -> Connected {
                    Connected
                }
            }

            state Connected {}
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("typestate pattern on-handler should parse");

    let typestate = match &module.top_level_items[0] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };
    let state = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::State(state) if state.name == "AwaitAuth" => Some(state),
            _ => None,
        })
        .expect("expected AwaitAuth state");
    let handler = state
        .items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Handler(handler) => Some(handler),
            _ => None,
        })
        .expect("expected pattern-form on-handler");

    assert_eq!(handler.params.len(), 2);
    assert_eq!(handler.params[0].ident, "pending");
    assert!(matches!(
        handler.params[0].typ.kind,
        TypeExprKind::Named { ref ident, ref type_args, .. }
            if ident == "Pending" && type_args.len() == 1
    ));
    assert_eq!(handler.params[1].ident, "__response");
    assert!(matches!(
        handler.params[1].typ.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "AuthApproved"
    ));
}

#[test]
fn test_parse_typestate_handler_for_provenance_binding() {
    let source = r#"
        type AuthCheck = {}
        type AuthApproved = {}

        typestate Gateway {
            fn new() -> AwaitAuth { AwaitAuth {} }

            state AwaitAuth {
                on AuthApproved(ok) for AuthCheck(req) -> stay {
                    ok;
                    req;
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("typestate on-handler provenance form should parse");

    let typestate = match &module.top_level_items[2] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };
    let state = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::State(state) if state.name == "AwaitAuth" => Some(state),
            _ => None,
        })
        .expect("expected AwaitAuth state");
    let handler = state
        .items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Handler(handler) => Some(handler),
            _ => None,
        })
        .expect("expected on-handler");

    assert_eq!(handler.params.len(), 1);
    assert_eq!(handler.params[0].ident, "ok");
    let provenance = handler
        .provenance
        .as_ref()
        .expect("expected for-provenance binding");
    assert_eq!(provenance.param.ident, "req");
    assert!(provenance.request_site_label.is_none());
    assert!(matches!(
        provenance.param.typ.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "AuthCheck"
    ));
}

#[test]
fn test_parse_typestate_handler_for_labeled_provenance_and_labeled_request_call() {
    let source = r#"
        type AuthCheck = {}
        type AuthApproved = {}

        typestate Gateway {
            fn new() -> AwaitAuth { AwaitAuth {} }

            state AwaitAuth {
                on AuthApproved(ok) for AuthCheck:auth1(req) {
                    let pending = request:auth1(self, req);
                    pending;
                    ok;
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("labeled provenance/request forms should parse");

    let typestate = match &module.top_level_items[2] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };
    let state = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::State(state) if state.name == "AwaitAuth" => Some(state),
            _ => None,
        })
        .expect("expected AwaitAuth state");
    let handler = state
        .items
        .iter()
        .find_map(|item| match item {
            TypestateStateItem::Handler(handler) => Some(handler),
            _ => None,
        })
        .expect("expected on-handler");

    let provenance = handler
        .provenance
        .as_ref()
        .expect("expected labeled for-provenance binding");
    assert_eq!(provenance.request_site_label.as_deref(), Some("auth1"));
    assert_eq!(provenance.param.ident, "req");
}

#[test]
fn test_parse_typestate_handler_shorthand_and_stay_forms() {
    let source = r#"
        type Ping = {}
        type Pong = {}

        typestate Gateway {
            fn new() -> Ready { Ready {} }

            state Ready {
                on Ping(p) -> stay {
                    send(0, Ping {});
                    request(0, Ping {});
                }

                on Pong {
                }
            }
        }
    "#;

    let module = parse_module_with_options(
        source,
        ParserOptions {
            experimental_typestate: true,
        },
    )
    .expect("typestate shorthand forms should parse");

    let typestate = match &module.top_level_items[2] {
        TopLevelItem::TypestateDef(def) => def,
        _ => panic!("expected typestate top-level item"),
    };
    let state = typestate
        .items
        .iter()
        .find_map(|item| match item {
            TypestateItem::State(state) if state.name == "Ready" => Some(state),
            _ => None,
        })
        .expect("expected Ready state");
    let mut handlers = state.items.iter().filter_map(|item| match item {
        TypestateStateItem::Handler(handler) => Some(handler),
        _ => None,
    });
    let ping_handler = handlers.next().expect("expected Ping handler");
    let pong_handler = handlers.next().expect("expected Pong handler");
    assert!(handlers.next().is_none(), "expected exactly two handlers");

    assert!(matches!(
        ping_handler.selector_ty.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "Ping"
    ));
    assert_eq!(ping_handler.params.len(), 1);
    assert_eq!(ping_handler.params[0].ident, "p");
    assert!(matches!(
        ping_handler.params[0].typ.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "Ping"
    ));
    assert!(matches!(
        ping_handler.ret_ty_expr.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "stay"
    ));

    assert!(matches!(
        pong_handler.selector_ty.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "Pong"
    ));
    assert!(pong_handler.params.is_empty());
    assert!(matches!(
        pong_handler.ret_ty_expr.kind,
        TypeExprKind::Named { ref ident, .. } if ident == "stay"
    ));
}
