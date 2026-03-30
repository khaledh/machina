use super::*;
use crate::core::ast::RefinementKind;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::ParseErrorKind;

fn parse_module(source: &str) -> Result<Module, ParseError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new_with_id_gen(&tokens, NodeIdGen::new());
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
fn test_parse_doc_comment_attaches_to_func_def() {
    let source = r#"
        /// Add two numbers.
        fn add(a: u64, b: u64) -> u64 { a + b }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let doc = func.doc.as_ref().expect("expected doc comment");
    assert_eq!(doc.raw, "Add two numbers.");
}

#[test]
fn test_parse_doc_comment_attaches_through_attributes() {
    let source = r#"
        /// Public adder.
        @public
        fn add(a: u64, b: u64) -> u64 { a + b }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let doc = func.doc.as_ref().expect("expected doc comment");
    assert_eq!(doc.raw, "Public adder.");
}

#[test]
fn test_parse_doc_comment_attaches_to_method_decl() {
    let source = r#"
        Thing :: {
            /// Start the thing.
            fn start(self);
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let block = module.method_blocks()[0];
    let MethodItem::Decl(method_decl) = &block.method_items[0] else {
        panic!("Expected method decl");
    };
    let doc = method_decl.doc.as_ref().expect("expected doc comment");
    assert_eq!(doc.raw, "Start the thing.");
}

#[test]
fn test_parse_doc_comment_with_blank_line_does_not_attach() {
    let source = r#"
        /// Detached docs.

        fn add(a: u64, b: u64) -> u64 { a + b }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    assert!(func.doc.is_none(), "expected blank-line doc to be ignored");
}

#[test]
fn test_parse_send_statement_as_emit_send() {
    let source = r#"
        fn test(target: u64) {
            send(target, 42);
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _tail) = block_parts(&func.body);
    let expr = match &items[0] {
        BlockItem::Expr(expr) => expr,
        _ => panic!("Expected block expr item"),
    };

    match &expr.kind {
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } => {
                match &to.kind {
                    ExprKind::Var { ident } => assert_eq!(ident, "target"),
                    _ => panic!("Expected send target var"),
                }
                match &payload.kind {
                    ExprKind::IntLit(value) => assert_eq!(*value, 42),
                    _ => panic!("Expected send payload int literal"),
                }
            }
            _ => panic!("Expected EmitKind::Send"),
        },
        _ => panic!("Expected send statement to lower to ExprKind::Emit"),
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
fn test_parse_nullable_address_types() {
    let source = r#"
        fn boot(base: paddr, next: vaddr?) -> paddr? {
            ()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &func.sig.params[0].typ.kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "paddr"),
        other => panic!("expected paddr param type, got {other:?}"),
    }
    match &func.sig.params[1].typ.kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "vaddr?"),
        other => panic!("expected vaddr? param type, got {other:?}"),
    }
    match &func.sig.ret_ty_expr.kind {
        TypeExprKind::Named { ident, .. } => assert_eq!(ident, "paddr?"),
        other => panic!("expected paddr? return type, got {other:?}"),
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
fn test_parse_defer_statement() {
    let source = r#"
        fn demo() {
            defer cleanup();
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);
    assert!(tail.is_none(), "expected no block tail");
    assert_eq!(items.len(), 1);

    let stmt = block_stmt_at(items, 0);
    match &stmt.kind {
        StmtExprKind::Defer { value } => match &value.kind {
            ExprKind::Call { callee, .. } => match &callee.kind {
                ExprKind::Var { ident, .. } => assert_eq!(ident, "cleanup"),
                _ => panic!("expected deferred callee var"),
            },
            _ => panic!("expected deferred call expression"),
        },
        _ => panic!("expected defer statement"),
    }
}

#[test]
fn test_parse_using_statement_with_block_body() {
    let source = r#"
        fn demo() {
            using file = open_read("notes.txt")? {
                let text = file.text().read_all()?;
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);
    assert!(tail.is_none(), "expected no block tail");
    assert_eq!(items.len(), 1);

    let stmt = block_stmt_at(items, 0);
    match &stmt.kind {
        StmtExprKind::Using {
            binding,
            value,
            body,
        } => {
            assert_eq!(binding.ident, "file");
            assert!(
                matches!(value.kind, ExprKind::Try { .. }),
                "expected using initializer to preserve `?` expression"
            );
            let (body_items, body_tail) = block_parts(body);
            assert!(body_tail.is_none(), "expected using body without tail");
            assert_eq!(body_items.len(), 1);
            assert!(
                matches!(
                    block_stmt_at(body_items, 0).kind,
                    StmtExprKind::LetBind { .. }
                ),
                "expected using body to contain let statement"
            );
        }
        _ => panic!("expected using statement"),
    }
}

#[test]
fn test_parse_using_initializer_still_allows_non_block_ternary() {
    let source = r#"
        fn demo(flag: bool) {
            using value = flag ? 1 : 2 {
                let copy = value;
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);
    assert!(tail.is_none(), "expected no block tail");
    assert_eq!(items.len(), 1);

    let stmt = block_stmt_at(items, 0);
    match &stmt.kind {
        StmtExprKind::Using { value, .. } => {
            assert!(
                matches!(value.kind, ExprKind::If { .. }),
                "expected ternary initializer to parse as if expression"
            );
        }
        _ => panic!("expected using statement"),
    }
}

#[test]
fn test_parse_pipe_expr_desugars_to_call_with_prepended_arg() {
    let source = r#"
        fn main(value: u64) -> u64 {
            value |> wrap(1, 2)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &block_tail(&func.body).kind {
        ExprKind::Call { callee, args } => {
            match &callee.kind {
                ExprKind::Var { ident } => assert_eq!(ident, "wrap"),
                _ => panic!("Expected piped call callee var"),
            }
            assert_eq!(args.len(), 3);
            match &args[0].expr.kind {
                ExprKind::Var { ident } => assert_eq!(ident, "value"),
                _ => panic!("Expected lhs to become first arg"),
            }
            assert!(matches!(args[1].expr.kind, ExprKind::IntLit(1)));
            assert!(matches!(args[2].expr.kind, ExprKind::IntLit(2)));
        }
        _ => panic!("Expected pipe expression to desugar to Call"),
    }
}

#[test]
fn test_parse_pipe_expr_chains_left_associatively() {
    let source = r#"
        fn main(value: u64) -> u64 {
            value |> f() |> g()
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    match &block_tail(&func.body).kind {
        ExprKind::Call { callee, args } => {
            match &callee.kind {
                ExprKind::Var { ident } => assert_eq!(ident, "g"),
                _ => panic!("Expected outer piped call callee var"),
            }
            assert_eq!(args.len(), 1);
            match &args[0].expr.kind {
                ExprKind::Call {
                    callee: inner_callee,
                    args: inner_args,
                } => {
                    match &inner_callee.kind {
                        ExprKind::Var { ident } => assert_eq!(ident, "f"),
                        _ => panic!("Expected inner piped callee var"),
                    }
                    assert_eq!(inner_args.len(), 1);
                    match &inner_args[0].expr.kind {
                        ExprKind::Var { ident } => assert_eq!(ident, "value"),
                        _ => panic!("Expected lhs to flow into first pipe stage"),
                    }
                }
                _ => panic!("Expected chained pipe stage to remain nested call"),
            }
        }
        _ => panic!("Expected chained pipe expression to desugar to Call"),
    }
}

#[test]
fn test_parse_pipe_expr_has_lowest_precedence_in_let_binding() {
    let source = r#"
        fn main(value: u64) -> u64 {
            let mapped = value + 1 |> wrap();
            mapped
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    match &stmt.kind {
        StmtExprKind::LetBind { value, .. } => match &value.kind {
            ExprKind::Call { callee, args } => {
                match &callee.kind {
                    ExprKind::Var { ident } => assert_eq!(ident, "wrap"),
                    _ => panic!("Expected let initializer to be a piped call"),
                }
                assert_eq!(args.len(), 1);
                match &args[0].expr.kind {
                    ExprKind::BinOp { op, .. } => assert_eq!(*op, BinaryOp::Add),
                    _ => panic!("Expected full lhs expr to become first arg"),
                }
            }
            _ => panic!("Expected let initializer to parse as desugared call"),
        },
        _ => panic!("Expected let binding"),
    }
}

#[test]
fn test_parse_pipe_expr_requires_call_rhs() {
    let source = r#"
        fn main(value: u64) -> u64 {
            value |> wrap
        }
    "#;

    let err = parse_source(source).expect_err("Expected parse error");
    assert!(matches!(err.kind(), ParseErrorKind::PipeRhsMustBeCall));
}

#[test]
fn test_parse_pipe_expr_rejects_method_rhs() {
    let source = r#"
        fn main(value: u64, writer: TextWriter) -> () {
            value |> writer.write_all()
        }
    "#;

    let err = parse_source(source).expect_err("Expected parse error");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::UnsupportedPipeMethodCall
    ));
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
fn test_parse_role_projection_expr() {
    let source = r#"
        fn main() -> () {
            PullRequest as Author
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::RoleProjection {
            type_name,
            role_name,
        } => {
            assert_eq!(type_name, "PullRequest");
            assert_eq!(role_name, "Author");
        }
        _ => panic!("Expected role projection expr"),
    }
}

#[test]
fn test_parse_role_projection_in_method_call_arg() {
    let source = r#"
        fn main(service: PRService) -> () {
            service.create(PullRequest as Author)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    match &tail.kind {
        ExprKind::MethodCall {
            method_name, args, ..
        } => {
            assert_eq!(method_name, "create");
            assert_eq!(args.len(), 1);
            match &args[0].expr.kind {
                ExprKind::RoleProjection {
                    type_name,
                    role_name,
                } => {
                    assert_eq!(type_name, "PullRequest");
                    assert_eq!(role_name, "Author");
                }
                _ => panic!("Expected role projection arg"),
            }
        }
        _ => panic!("Expected method call"),
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
fn test_parse_struct_update_field_shorthand() {
    let source = r#"
        type Point = { x: u64, y: u64, color: u64 }

        fn main() -> Point {
            let p = Point { x: 1, y: 2, color: 3 };
            let x = 10;
            { p | x, color: 5 }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);
    assert_eq!(items.len(), 2);

    let tail = tail.expect("Expected block tail");
    let ExprKind::StructUpdate { fields, .. } = &tail.kind else {
        panic!("Expected StructUpdate expression");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "x");
    assert_eq!(fields[1].name, "color");

    match &fields[0].value.kind {
        ExprKind::Var { ident, .. } => assert_eq!(ident, "x"),
        _ => panic!("Expected shorthand field value to parse as Var"),
    }
    match &fields[1].value.kind {
        ExprKind::IntLit(value) => assert_eq!(*value, 5),
        _ => panic!("Expected explicit field value"),
    }
}

#[test]
fn test_parse_struct_lit_field_shorthand() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn main() -> Point {
            let x = 1;
            let y = 2;
            Point { x, y }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];

    let tail = block_tail(&func.body);
    let ExprKind::StructLit { fields, .. } = &tail.kind else {
        panic!("Expected StructLit expression");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "x");
    assert_eq!(fields[1].name, "y");

    match &fields[0].value.kind {
        ExprKind::Var { ident, .. } => assert_eq!(ident, "x"),
        _ => panic!("Expected shorthand field value to parse as Var"),
    }
    match &fields[1].value.kind {
        ExprKind::Var { ident, .. } => assert_eq!(ident, "y"),
        _ => panic!("Expected shorthand field value to parse as Var"),
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
fn test_parse_named_call_args() {
    let source = r#"
        fn test() -> u64 {
            connect("example.com", timeout: 30, port: 8080)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    let ExprKind::Call { callee, args } = &tail.kind else {
        panic!("Expected Call");
    };
    match &callee.kind {
        ExprKind::Var { ident, .. } => assert_eq!(ident, "connect"),
        _ => panic!("Expected call callee var"),
    }
    assert_eq!(args.len(), 3);
    assert!(args[0].label.is_none());
    assert_eq!(args[1].label.as_ref().unwrap().name, "timeout");
    assert_eq!(args[2].label.as_ref().unwrap().name, "port");
}

#[test]
fn test_parse_named_call_arg_with_mode() {
    let source = r#"
        fn test(x: u64, y: u64) -> () {
            swap(a: inout x, b: inout y)
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    let ExprKind::Call { args, .. } = &tail.kind else {
        panic!("Expected Call");
    };
    assert_eq!(args.len(), 2);
    assert_eq!(args[0].label.as_ref().unwrap().name, "a");
    assert_eq!(args[1].label.as_ref().unwrap().name, "b");
    assert!(matches!(args[0].mode, CallArgMode::InOut));
    assert!(matches!(args[1].mode, CallArgMode::InOut));
}

#[test]
fn test_parse_rejects_positional_arg_after_named_arg() {
    let source = r#"
        fn test() -> u64 {
            connect(host: "example.com", 8080)
        }
    "#;

    let err = parse_source(source).expect_err("expected parse failure");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::PositionalArgAfterNamedArg
    ));
}

#[test]
fn test_parse_function_param_default_value() {
    let source = r#"
        fn connect(host: string, port: u64 = 443, timeout: u64 = 30) -> u64 {
            port + timeout
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let sig = &funcs[0].sig;

    assert_eq!(sig.params.len(), 3);
    assert!(sig.params[0].default.is_none());
    assert!(matches!(
        sig.params[1].default.as_ref().map(|expr| &expr.kind),
        Some(ExprKind::IntLit(443))
    ));
    assert!(matches!(
        sig.params[2].default.as_ref().map(|expr| &expr.kind),
        Some(ExprKind::IntLit(30))
    ));
}

#[test]
fn test_parse_method_param_default_value() {
    let source = r#"
        Config :: {
            fn connect(self, port: u64 = 443) -> u64 {
                port
            }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let method_block = module.method_blocks()[0];
    let method = match &method_block.method_items[0] {
        MethodItem::Def(def) => def,
        _ => panic!("Expected method def"),
    };

    assert_eq!(method.sig.params.len(), 1);
    assert!(matches!(
        method.sig.params[0].default.as_ref().map(|expr| &expr.kind),
        Some(ExprKind::IntLit(443))
    ));
}

#[test]
fn test_parse_rejects_default_on_non_in_param() {
    let source = r#"
        fn fill(out dst: u64 = 0) -> () {}
    "#;

    let err = parse_source(source).expect_err("expected parse failure");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::DefaultValueRequiresInParam
    ));
}

#[test]
fn test_parse_rejects_nondefault_param_after_default() {
    let source = r#"
        fn connect(host: string = "example.com", port: u64) -> u64 {
            port
        }
    "#;

    let err = parse_source(source).expect_err("expected parse failure");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::NonDefaultParamAfterDefault { .. }
    ));
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
fn test_parse_array_pattern_with_trailing_rest() {
    let source = r#"
        fn test(argv: string[*]) {
            let [path, needle, ...] = argv;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    let StmtExprKind::LetBind { pattern, .. } = &stmt.kind else {
        panic!("Expected let binding");
    };

    match &pattern.kind {
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => {
            assert_eq!(prefix.len(), 2);
            assert!(suffix.is_empty());
            assert!(rest.is_some());
            assert!(rest.as_ref().is_some_and(|rest| rest.pattern.is_none()));
        }
        other => panic!("Expected array pattern, got {:?}", other),
    }
}

#[test]
fn test_parse_wildcard_bind_pattern() {
    let source = r#"
        fn test(argv: string[*]) {
            let _ = argv;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    let StmtExprKind::LetBind { pattern, .. } = &stmt.kind else {
        panic!("Expected let binding");
    };

    assert!(matches!(pattern.kind, BindPatternKind::Wildcard));
}

#[test]
fn test_parse_array_pattern_with_wildcard_and_rest() {
    let source = r#"
        fn test(argv: string[*]) {
            let [_, path, needle, ...] = argv;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    let StmtExprKind::LetBind { pattern, .. } = &stmt.kind else {
        panic!("Expected let binding");
    };

    match &pattern.kind {
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => {
            assert_eq!(prefix.len(), 3);
            assert!(matches!(prefix[0].kind, BindPatternKind::Wildcard));
            assert!(matches!(prefix[1].kind, BindPatternKind::Name { .. }));
            assert!(matches!(prefix[2].kind, BindPatternKind::Name { .. }));
            assert!(suffix.is_empty());
            assert!(rest.as_ref().is_some_and(|rest| rest.pattern.is_none()));
        }
        other => panic!("Expected array pattern, got {:?}", other),
    }
}

#[test]
fn test_parse_array_pattern_with_middle_rest() {
    let source = r#"
        fn test(argv: string[*]) {
            let [head, ...middle, tail] = argv;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    let StmtExprKind::LetBind { pattern, .. } = &stmt.kind else {
        panic!("Expected let binding");
    };

    match &pattern.kind {
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => {
            assert_eq!(prefix.len(), 1);
            assert_eq!(suffix.len(), 1);
            let rest_pattern = rest
                .as_ref()
                .and_then(|rest| rest.pattern.as_deref())
                .expect("expected named rest binding");
            match &rest_pattern.kind {
                BindPatternKind::Name { ident, .. } => assert_eq!(ident, "middle"),
                other => panic!("Expected name rest pattern, got {:?}", other),
            }
        }
        other => panic!("Expected array pattern, got {:?}", other),
    }
}

#[test]
fn test_parse_array_pattern_with_leading_rest() {
    let source = r#"
        fn test(argv: string[*]) {
            let [...prefix, tail] = argv;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 0);

    let StmtExprKind::LetBind { pattern, .. } = &stmt.kind else {
        panic!("Expected let binding");
    };

    match &pattern.kind {
        BindPatternKind::Array {
            prefix,
            rest,
            suffix,
        } => {
            assert!(prefix.is_empty());
            assert_eq!(suffix.len(), 1);
            let rest_pattern = rest
                .as_ref()
                .and_then(|rest| rest.pattern.as_deref())
                .expect("expected named rest binding");
            match &rest_pattern.kind {
                BindPatternKind::Name { ident, .. } => assert_eq!(ident, "prefix"),
                other => panic!("Expected name rest pattern, got {:?}", other),
            }
        }
        other => panic!("Expected array pattern, got {:?}", other),
    }
}

#[test]
fn test_parse_array_pattern_rejects_multiple_rest_elements() {
    let source = r#"
        fn test(argv: string[*]) {
            let [head, ...middle, ...tail] = argv;
        }
    "#;

    let err = parse_source(source).expect_err("Expected parse error");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::DuplicateArrayRestPattern
    ));
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
            assert_eq!(arms[0].patterns.len(), 1);
            assert_eq!(arms[1].patterns.len(), 1);
            assert_eq!(arms[2].patterns.len(), 1);

            match &arms[0].patterns[0] {
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

            match &arms[1].patterns[0] {
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

            assert!(matches!(arms[2].patterns[0], MatchPattern::Wildcard { .. }));
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
    } = &arms[0].patterns[0]
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
            assert_eq!(arms[0].patterns.len(), 1);
            assert_eq!(arms[1].patterns.len(), 1);

            match &arms[0].patterns[0] {
                MatchPattern::BoolLit { value, .. } => assert!(*value),
                _ => panic!("Expected bool literal pattern in arm 0"),
            }

            match &arms[1].patterns[0] {
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
            assert_eq!(arms[0].patterns.len(), 1);
            assert_eq!(arms[1].patterns.len(), 1);

            match &arms[0].patterns[0] {
                MatchPattern::IntLit { value, .. } => assert_eq!(*value, 0),
                _ => panic!("Expected int literal pattern in arm 0"),
            }

            match &arms[1].patterns[0] {
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
            assert_eq!(arms[0].patterns.len(), 1);

            match &arms[0].patterns[0] {
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
            assert_eq!(arms[0].patterns.len(), 1);

            match &arms[0].patterns[0] {
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
            assert_eq!(arms[0].patterns.len(), 1);
            assert_eq!(arms[1].patterns.len(), 1);

            match &arms[0].patterns[0] {
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
            assert!(matches!(arms[1].patterns[0], MatchPattern::Wildcard { .. }));
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
            assert_eq!(arms[0].patterns.len(), 1);
            match &arms[0].patterns[0] {
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
fn test_parse_match_expr_alternation_patterns() {
    let source = r#"
        fn test(x: u8) -> bool {
            match x {
                32 | 10 | 9 | 13 => true,
                _ => false,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Match { arms, .. } => {
            assert_eq!(arms.len(), 2);
            assert_eq!(arms[0].patterns.len(), 4);
            assert!(
                matches!(&arms[0].patterns[0], MatchPattern::IntLit { value, .. } if *value == 32)
            );
            assert!(
                matches!(&arms[0].patterns[1], MatchPattern::IntLit { value, .. } if *value == 10)
            );
            assert!(
                matches!(&arms[0].patterns[2], MatchPattern::IntLit { value, .. } if *value == 9)
            );
            assert!(
                matches!(&arms[0].patterns[3], MatchPattern::IntLit { value, .. } if *value == 13)
            );
            assert!(matches!(arms[1].patterns[0], MatchPattern::Wildcard { .. }));
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_lowercase_binding_pattern() {
    let source = r#"
        fn test(result: u64 | ParseError) -> u64 | ParseError {
            match result {
                ok: u64 => ok,
                other => other,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    match &tail.kind {
        ExprKind::Match { arms, .. } => {
            assert_eq!(arms.len(), 2);
            match &arms[1].patterns[0] {
                MatchPattern::Binding { ident, .. } => assert_eq!(ident, "other"),
                other => panic!("Expected binding pattern, got {other:?}"),
            }
        }
        _ => panic!("Expected match expression"),
    }
}

#[test]
fn test_parse_match_expr_alternation_rejects_binding_patterns() {
    let source = r#"
        fn test(t: (u64, u64)) -> u64 {
            match t {
                (x, _) | (1, 2) => x,
                _ => 0,
            }
        }
    "#;

    match parse_source(source) {
        Err(err)
            if matches!(
                err.kind(),
                ParseErrorKind::UnsupportedMatchAlternationPattern
            ) => {}
        other => panic!(
            "Expected UnsupportedMatchAlternationPattern, got {:?}",
            other
        ),
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
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            assert!(on_error.is_none());
            assert!(matches!(&fallible_expr.kind, ExprKind::Call { .. }));
        }
        other => panic!("Expected try expression, got {other:?}"),
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
            if true { }
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
            if true { } else if false { } else { }
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
fn test_parse_statement_position_if_without_semicolon() {
    let source = r#"
        fn test() {
            if true { }
            let x = 1;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);

    assert_eq!(items.len(), 2);
    assert!(tail.is_none());
    assert!(matches!(
        items[0],
        BlockItem::Expr(Expr {
            kind: ExprKind::If { .. },
            ..
        })
    ));
    assert!(matches!(
        items[1],
        BlockItem::Stmt(StmtExpr {
            kind: StmtExprKind::LetBind { .. },
            ..
        })
    ));
}

#[test]
fn test_parse_statement_position_match_without_semicolon() {
    let source = r#"
        fn test(x: u64) {
            match x {
                0 => (),
                _ => (),
            }
            let y = 2;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);

    assert_eq!(items.len(), 2);
    assert!(tail.is_none());
    assert!(matches!(
        items[0],
        BlockItem::Expr(Expr {
            kind: ExprKind::Match { .. },
            ..
        })
    ));
    assert!(matches!(
        items[1],
        BlockItem::Stmt(StmtExpr {
            kind: StmtExprKind::LetBind { .. },
            ..
        })
    ));
}

#[test]
fn test_parse_statement_position_block_without_semicolon() {
    let source = r#"
        fn test() {
            { let x = 1; }
            let y = 2;
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);

    assert_eq!(items.len(), 2);
    assert!(tail.is_none());
    assert!(matches!(
        items[0],
        BlockItem::Expr(Expr {
            kind: ExprKind::Block { .. },
            ..
        })
    ));
    assert!(matches!(
        items[1],
        BlockItem::Stmt(StmtExpr {
            kind: StmtExprKind::LetBind { .. },
            ..
        })
    ));
}

#[test]
fn test_parse_if_expression_tail_with_continuation() {
    let source = r#"
        fn test(z: u64) -> u64 {
            if true { 1 } else { 2 } + z
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);

    assert!(matches!(tail.kind, ExprKind::BinOp { .. }));
}

#[test]
fn test_parse_plain_expr_still_requires_semicolon_between_statements() {
    let source = r#"
        fn test() {
            foo()
            let x = 1;
        }
    "#;

    let err = parse_source(source).expect_err("Expected parse error");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::ExpectedToken(TokenKind::RBrace, _)
    ));
}

#[test]
fn test_parse_statement_position_if_rejects_trailing_semicolon() {
    let source = r#"
        fn test() {
            if true { };
            let x = 1;
        }
    "#;

    let err = parse_source(source).expect_err("Expected parse error");
    assert!(matches!(
        err.kind(),
        ParseErrorKind::TrailingSemicolonAfterBlockStmt
    ));
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
        _ => panic!("Expected string attribute arg"),
    }
}

#[test]
fn test_parse_layout_attr_with_named_size_arg() {
    let source = r#"
        @layout(fixed, size: 24)
        type Foo = {
            a: u64,
            b: u64,
            c: u64,
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_def = module.type_defs()[0];
    assert_eq!(type_def.attrs.len(), 1);
    assert_eq!(type_def.attrs[0].name, "layout");
    assert_eq!(type_def.attrs[0].args.len(), 2);
    assert!(matches!(
        &type_def.attrs[0].args[0],
        AttrArg::Ident(value) if value == "fixed"
    ));
    assert!(matches!(
        &type_def.attrs[0].args[1],
        AttrArg::Named { name, value }
            if name == "size" && matches!(value.as_ref(), AttrArg::Int(24))
    ));
}

#[test]
fn test_parse_struct_field_align_attr() {
    let source = r#"
        @layout(fixed)
        type Foo = {
            @align(8)
            data: u64,
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_def = module.type_defs()[0];
    let TypeDefKind::Struct { fields } = &type_def.kind else {
        panic!("Expected struct type");
    };
    assert_eq!(fields.len(), 1);
    assert_eq!(fields[0].attrs.len(), 1);
    assert_eq!(fields[0].attrs[0].name, "align");
    assert!(matches!(
        fields[0].attrs[0].args.as_slice(),
        [AttrArg::Int(8)]
    ));
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
fn test_parse_generic_method_block() {
    let source = r#"
        type Box<T> = { value: T }
        Box<T> :: {
            fn value_of(self) -> T { self.value }
        }
    "#;
    let module = parse_module(source).expect("Failed to parse");
    let method_blocks = module.method_blocks();

    assert_eq!(method_blocks.len(), 1);
    assert_eq!(method_blocks[0].type_name, "Box");
    assert_eq!(method_blocks[0].type_args.len(), 1);
    assert!(matches!(
        method_blocks[0].type_args[0].kind,
        TypeExprKind::Named { ref ident, ref type_args } if ident == "T" && type_args.is_empty()
    ));
}

#[test]
fn test_parse_attr_on_method_block_rejected() {
    let source = "@intrinsic Foo :: { fn bar(self) { 1 } }";
    let result = parse_module(source);

    assert!(matches!(
        result,
        Err(err) if matches!(err.kind(), ParseErrorKind::AttributeNotAllowed)
    ));
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
fn test_parse_compound_assignment_stmt_variant() {
    let source = r#"
        fn test() -> u64 {
            var x = 1;
            x += 2;
            x
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, tail) = block_parts(&func.body);

    let stmt = block_stmt_at(items, 1);
    let StmtExprKind::CompoundAssign {
        assignee,
        op,
        value,
        ..
    } = &stmt.kind
    else {
        panic!("Expected compound assignment statement");
    };
    assert_eq!(*op, BinaryOp::Add);
    assert!(matches!(assignee.kind, ExprKind::Var { .. }));
    assert!(matches!(value.kind, ExprKind::IntLit(2)));
    assert!(tail.is_some(), "Expected tail expression");
}

#[test]
fn test_parse_shift_compound_assignment() {
    let source = r#"
        fn test() -> u64 {
            var x = 1;
            x <<= 3;
            x
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let (items, _) = block_parts(&func.body);
    let stmt = block_stmt_at(items, 1);
    let StmtExprKind::CompoundAssign { op, value, .. } = &stmt.kind else {
        panic!("Expected compound assignment statement");
    };
    assert_eq!(*op, BinaryOp::Shl);
    assert!(matches!(value.kind, ExprKind::IntLit(3)));
}

#[test]
fn test_parse_ternary_expr_desugars_to_if_expr() {
    let source = r#"
        fn test() -> u64 {
            let flag = true;
            flag ? 1 : 2
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::If {
        cond,
        then_body,
        else_body,
    } = &tail.kind
    else {
        panic!("Expected ternary to parse as If expr");
    };
    assert!(matches!(cond.kind, ExprKind::Var { .. }));
    assert!(matches!(then_body.kind, ExprKind::IntLit(1)));
    assert!(matches!(else_body.kind, ExprKind::IntLit(2)));
}

#[test]
fn test_parse_ternary_precedence_with_binary_condition() {
    let source = r#"
        fn test() -> u64 {
            1 + 2 * 3 > 0 ? 4 : 5
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::If { cond, .. } = &tail.kind else {
        panic!("Expected ternary If expr");
    };
    let ExprKind::BinOp { op, .. } = &cond.kind else {
        panic!("Expected binary condition");
    };
    assert_eq!(*op, BinaryOp::Gt);
}

#[test]
fn test_parse_nested_ternary_is_right_associative() {
    let source = r#"
        fn test() -> u64 {
            a ? b : c ? d : e
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::If { else_body, .. } = &tail.kind else {
        panic!("Expected outer ternary If expr");
    };
    assert!(
        matches!(else_body.kind, ExprKind::If { .. }),
        "Expected nested ternary in else-branch"
    );
}

#[test]
fn test_parse_try_postfix_still_works() {
    let source = r#"
        fn test() -> u64 | ParseErr {
            parse_u64("42")?
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::Try {
        fallible_expr,
        on_error,
    } = &tail.kind
    else {
        panic!("Expected try expression");
    };
    assert!(on_error.is_none());
    assert!(
        matches!(fallible_expr.kind, ExprKind::Call { .. }),
        "Expected try operand to be a call"
    );
}

#[test]
fn test_parse_try_or_with_closure_handler() {
    let source = r#"
        fn test() -> u64 {
            parse_u64("42") or |err| { err; 0 }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::Try {
        fallible_expr,
        on_error,
    } = &tail.kind
    else {
        panic!("Expected try expression");
    };
    assert!(
        matches!(fallible_expr.kind, ExprKind::Call { .. }),
        "Expected fallible expression to be a call"
    );
    let Some(handler) = on_error else {
        panic!("Expected try-or to carry an on_error handler");
    };
    assert!(
        matches!(handler.kind, ExprKind::Closure { .. }),
        "Expected on_error handler to be parsed as a closure expression"
    );
}

#[test]
fn test_parse_try_or_block_sugar_wraps_handler_closure() {
    let source = r#"
        fn test() -> u64 {
            parse_u64("42") or { 0 }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = &funcs[0];
    let tail = block_tail(&func.body);
    let ExprKind::Try {
        fallible_expr,
        on_error,
    } = &tail.kind
    else {
        panic!("Expected try expression");
    };
    assert!(matches!(fallible_expr.kind, ExprKind::Call { .. }));
    let Some(handler) = on_error else {
        panic!("Expected handler closure");
    };
    let ExprKind::Closure { params, body, .. } = &handler.kind else {
        panic!("Expected closure handler");
    };
    assert_eq!(params.len(), 1);
    assert!(matches!(body.kind, ExprKind::Block { .. }));
}

#[test]
fn test_parse_try_or_arm_sugar_wraps_match_handler_closure() {
    let source = r#"
        type IoError = { code: u64 }
        type ParseError = { line: u64 }

        fn test() -> u64 {
            read() or {
                value: u64 => value,
                io: IoError => io.code,
                parse: ParseError => parse.line,
            }
        }
    "#;

    let funcs = parse_source(source).expect("Failed to parse");
    let func = funcs
        .iter()
        .find(|func| func.sig.name == "test")
        .expect("Missing test function");
    let tail = block_tail(&func.body);
    let ExprKind::Try {
        on_error: Some(handler),
        ..
    } = &tail.kind
    else {
        panic!("Expected try expression with handler");
    };
    let ExprKind::Closure { body, .. } = &handler.kind else {
        panic!("Expected closure handler");
    };
    let ExprKind::Block {
        tail: Some(match_tail),
        ..
    } = &body.kind
    else {
        panic!("Expected closure body block");
    };
    let ExprKind::Match { arms, .. } = &match_tail.kind else {
        panic!("Expected match expression in arm-style sugar");
    };
    assert_eq!(arms.len(), 3);
}

#[test]
fn test_parse_linear_type_blocks() {
    let source = r#"
        @linear
        type Door = {
            id: u64,

            states {
                Closed,
                @final Locked(u64),
            }

            actions {
                open: Closed -> Locked | DoorError,
            }

            triggers {
                timeout: Locked -> Closed,
            }

            roles {
                Owner { open }
            }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let type_defs = module.type_defs();
    assert_eq!(type_defs.len(), 1);

    let type_def = type_defs[0];
    match &type_def.kind {
        TypeDefKind::Linear { linear } => {
            assert_eq!(linear.fields.len(), 1);
            assert_eq!(linear.fields[0].name, "id");

            assert_eq!(linear.states.len(), 2);
            assert_eq!(linear.states[0].name, "Closed");
            assert!(linear.states[0].payload.is_empty());
            assert_eq!(linear.states[1].name, "Locked");
            assert_eq!(linear.states[1].payload.len(), 1);
            assert_eq!(linear.states[1].attrs.len(), 1);
            assert_eq!(linear.states[1].attrs[0].name, "final");

            assert_eq!(linear.actions.len(), 1);
            assert_eq!(linear.actions[0].name, "open");
            assert_eq!(linear.actions[0].source_state, "Closed");
            assert_eq!(linear.actions[0].target_state, "Locked");
            assert_eq!(linear.actions[0].params.len(), 0);
            assert!(linear.actions[0].error_ty_expr.is_some());

            assert_eq!(linear.triggers.len(), 1);
            assert_eq!(linear.triggers[0].name, "timeout");
            assert_eq!(linear.triggers[0].source_state, "Locked");
            assert_eq!(linear.triggers[0].target_state, "Closed");

            assert_eq!(linear.roles.len(), 1);
            assert_eq!(linear.roles[0].name, "Owner");
            assert_eq!(linear.roles[0].allowed_actions, vec!["open".to_string()]);
        }
        _ => panic!("Expected linear type"),
    }
}

#[test]
fn test_parse_method_receiver_type_annotation() {
    let source = r#"
        Door :: {
            fn close(self: Closed) -> Closed;
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    let method_blocks = module.method_blocks();
    assert_eq!(method_blocks.len(), 1);
    let MethodItem::Decl(method_decl) = &method_blocks[0].method_items[0] else {
        panic!("Expected method declaration");
    };
    let receiver_ty_expr = method_decl
        .sig
        .self_param
        .receiver_ty_expr
        .as_ref()
        .expect("Expected receiver type annotation");
    match &receiver_ty_expr.kind {
        TypeExprKind::Named { ident, type_args } => {
            assert_eq!(ident, "Closed");
            assert!(type_args.is_empty());
        }
        _ => panic!("Expected named receiver type"),
    }
}

#[test]
fn test_parse_machine_host_def() {
    let source = r#"
        machine DoorService hosts Door(key: id) {
            fields { counter: u64 }

            action open(door, code: u64) -> Open {
                code;
                Open {}
            }

            trigger timeout(door) {
                door;
            }

            on Tick(tick: Tick) {
                tick;
            }
        }
    "#;

    let module = parse_module(source).expect("Failed to parse");
    assert_eq!(module.machine_defs().len(), 1);
    let machine = module.machine_defs()[0];
    assert_eq!(machine.name, "DoorService");
    assert_eq!(machine.host.type_name, "Door");
    assert_eq!(machine.host.key_field, "id");
    assert_eq!(machine.items.len(), 4);
}
