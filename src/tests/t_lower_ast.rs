use super::*;
use crate::context::AstContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::mcir::interner::GlobalInterner;
use crate::mcir::types::{GlobalItem, GlobalPayload, GlobalSection};
use crate::nrvo::NrvoAnalyzer;
use crate::parser::Parser;
use crate::resolve::resolve;
use crate::typeck::type_check;

fn analyze(source: &str) -> AnalyzedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");

    let ast_context = AstContext::new(module);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");

    NrvoAnalyzer::new(type_checked_context).analyze()
}

fn lower_body_with_globals(ctx: &AnalyzedContext, func: &Function) -> (FuncBody, Vec<GlobalItem>) {
    let mut interner = GlobalInterner::new();
    let mut lowerer = FuncLowerer::new(ctx, func, &mut interner);
    let body = lowerer.lower().expect("Failed to lower function");
    (body, interner.take())
}

#[test]
fn test_lower_literal_value() {
    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    println!("Lowered body:\n{}", body);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 1);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 42);
                }
                _ => panic!("unexpected assign src"),
            }
        }
        _ => panic!("unexpected stmt[0]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_string_literal_global() {
    let source = r#"
        fn main() -> string {
            "hi"
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let mut interner = GlobalInterner::new();
    let mut lowerer = FuncLowerer::new(&analyzed, func, &mut interner);

    let body = lowerer.lower().expect("Failed to lower function");
    let globals = interner.take();

    assert_eq!(globals.len(), 1);
    let g0 = &globals[0];
    assert_eq!(g0.kind, GlobalSection::RoData);
    match &g0.payload {
        GlobalPayload::String(s) => assert_eq!(s, "hi"),
        _ => panic!("expected string payload"),
    }

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 3);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { src, .. } => match src {
            Rvalue::Use(Operand::Const(Const::GlobalAddr { .. })) => {}
            _ => panic!("expected global addr for string ptr"),
        },
        _ => panic!("unexpected stmt[0]"),
    }

    match &entry_block.stmts[1] {
        Statement::CopyScalar { src, .. } => match src {
            Rvalue::Use(Operand::Const(Const::Int { value, bits, .. })) => {
                assert_eq!(*value, 2);
                assert_eq!(*bits, 32);
            }
            _ => panic!("expected len const"),
        },
        _ => panic!("unexpected stmt[1]"),
    }

    match &entry_block.stmts[2] {
        Statement::CopyScalar { src, .. } => match src {
            Rvalue::Use(Operand::Const(Const::Int { value, bits, .. })) => {
                assert_eq!(*value, 0);
                assert_eq!(*bits, 8);
            }
            _ => panic!("expected tag const"),
        },
        _ => panic!("unexpected stmt[2]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_call_emits_arg_temp() {
    let source = r#"
        fn id(x: u64) -> u64 {
            x
        }

        fn main() -> u64 {
            id(1)
        }
    "#;

    let analyzed = analyze(source);
    let func = *analyzed
        .module
        .funcs()
        .iter()
        .find(|f| f.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func);

    println!("Lowered body:\n{}", body);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 3);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(2));
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 1);
                }
                _ => panic!("unexpected assign src"),
            }
        }
        _ => panic!("unexpected stmt[0]"),
    }

    match &entry_block.stmts[1] {
        Statement::Call { dst, args, .. } => {
            match dst {
                PlaceAny::Scalar(place) => assert_eq!(place.base(), LocalId(1)),
                _ => panic!("unexpected call dst"),
            }
            assert_eq!(args.len(), 1);
            match &args[0] {
                PlaceAny::Scalar(place) => assert_eq!(place.base(), LocalId(2)),
                _ => panic!("unexpected call arg"),
            }
        }
        _ => panic!("unexpected stmt[1]"),
    }

    match &entry_block.stmts[2] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            match src {
                Rvalue::Use(Operand::Copy(place)) => {
                    assert_eq!(place.base(), LocalId(1));
                }
                _ => panic!("unexpected return src"),
            }
        }
        _ => panic!("unexpected stmt[2]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));

    let id_func = *analyzed
        .module
        .funcs()
        .iter()
        .find(|f| f.name == "id")
        .expect("id not found");
    let (body, _) = lower_body_with_globals(&analyzed, id_func);

    println!("Lowered body:\n{}", body);
}

#[test]
fn test_lower_tuple_return_literal() {
    let source = r#"
        fn main() -> (u64, bool) {
            (42, true)
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    println!("Lowered body:\n{}", body);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 2);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(dst.projections(), &[Projection::Field { index: 0 }]);
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 42);
                }
                _ => panic!("unexpected tuple field 0 src"),
            }
        }
        _ => panic!("unexpected stmt[0]"),
    }

    match &entry_block.stmts[1] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(dst.projections(), &[Projection::Field { index: 1 }]);
            match src {
                Rvalue::Use(Operand::Const(Const::Bool(value))) => {
                    assert_eq!(*value, true);
                }
                _ => panic!("unexpected tuple field 1 src"),
            }
        }
        _ => panic!("unexpected stmt[1]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_nrvo_binding_elides_copy() {
    let source = r#"
        fn main() -> (u64, bool) {
            let x = (42, true);
            x
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    println!("Lowered body:\n{}", body);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];

    assert_eq!(entry_block.stmts.len(), 2);
    assert!(
        !entry_block
            .stmts
            .iter()
            .any(|stmt| matches!(stmt, Statement::CopyAggregate { .. }))
    );

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, .. } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(dst.projections(), &[Projection::Field { index: 0 }]);
        }
        _ => panic!("unexpected stmt[0]"),
    }

    match &entry_block.stmts[1] {
        Statement::CopyScalar { dst, .. } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(dst.projections(), &[Projection::Field { index: 1 }]);
        }
        _ => panic!("unexpected stmt[1]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_struct_pattern_binding() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn main() -> u64 {
            let Point { x, y } = Point { x: 1, y: 2 };
            x + y
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed
        .module
        .funcs()
        .iter()
        .find(|f| f.name == "main")
        .copied()
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func);

    let x_local = body
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| local.name.as_deref() == Some("x"))
        .map(|(idx, _)| LocalId(idx as u32))
        .expect("x local not found");
    let y_local = body
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| local.name.as_deref() == Some("y"))
        .map(|(idx, _)| LocalId(idx as u32))
        .expect("y local not found");

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    let mut x_bound = false;
    let mut y_bound = false;

    for stmt in &entry_block.stmts {
        let Statement::CopyScalar { dst, src } = stmt else {
            continue;
        };
        let Rvalue::Use(Operand::Copy(place)) = src else {
            continue;
        };
        if dst.base() == x_local && dst.projections().is_empty() {
            if place.projections() == &[Projection::Field { index: 0 }] {
                x_bound = true;
            }
        }
        if dst.base() == y_local && dst.projections().is_empty() {
            if place.projections() == &[Projection::Field { index: 1 }] {
                y_bound = true;
            }
        }
    }

    assert!(x_bound, "expected struct field x to be bound");
    assert!(y_bound, "expected struct field y to be bound");
}

#[test]
fn test_lower_enum_variant_literal() {
    let source = r#"
        type Color = Red | Green | Blue

        fn main() -> Color {
            Color::Green
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 1);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 1);
                }
                _ => panic!("unexpected enum literal src"),
            }
        }
        _ => panic!("unexpected stmt[0]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_enum_variant_payload_literal() {
    let source = r#"
        type Pair = A(u64, bool) | B(u64, bool)

        fn main() -> Pair {
            Pair::B(7, true)
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    assert_eq!(entry_block.stmts.len(), 3);

    match &entry_block.stmts[0] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(dst.projections(), &[Projection::Field { index: 0 }]);
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 1);
                }
                _ => panic!("unexpected enum tag src"),
            }
        }
        _ => panic!("unexpected stmt[0]"),
    }

    match &entry_block.stmts[1] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(
                dst.projections(),
                &[
                    Projection::Field { index: 1 },
                    Projection::ByteOffset { offset: 0 }
                ]
            );
            match src {
                Rvalue::Use(Operand::Const(Const::Int { value, .. })) => {
                    assert_eq!(*value, 7);
                }
                _ => panic!("unexpected enum payload[0] src"),
            }
        }
        _ => panic!("unexpected stmt[1]"),
    }

    match &entry_block.stmts[2] {
        Statement::CopyScalar { dst, src } => {
            assert_eq!(dst.base(), LocalId(0));
            assert_eq!(
                dst.projections(),
                &[
                    Projection::Field { index: 1 },
                    Projection::ByteOffset { offset: 8 }
                ]
            );
            match src {
                Rvalue::Use(Operand::Const(Const::Bool(value))) => {
                    assert_eq!(*value, true);
                }
                _ => panic!("unexpected enum payload[1] src"),
            }
        }
        _ => panic!("unexpected stmt[2]"),
    }

    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_match_switch_payload_binding() {
    let source = r#"
        type Color = Red(u64) | Green

        fn main(c: Color) -> u64 {
            match c {
                Red(x) => x,
                Green => 0,
                _ => 1,
            }
        }
    "#;

    let analyzed = analyze(source);
    let func = *analyzed
        .module
        .funcs()
        .iter()
        .find(|f| f.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func);

    let c_local = body
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| local.name.as_deref() == Some("c"))
        .map(|(idx, _)| LocalId(idx as u32))
        .expect("c local not found");
    let x_local = body
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| local.name.as_deref() == Some("x"))
        .map(|(idx, _)| LocalId(idx as u32))
        .expect("x local not found");

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    match &entry_block.terminator {
        Terminator::Switch {
            discr,
            cases,
            default: _,
        } => {
            let mut values = cases.iter().map(|c| c.value).collect::<Vec<_>>();
            values.sort_unstable();
            assert_eq!(values, vec![0, 1]);
            match discr {
                Operand::Copy(place) => {
                    assert_eq!(place.base(), c_local);
                    assert_eq!(place.projections(), &[Projection::Field { index: 0 }]);
                }
                _ => panic!("unexpected match discr"),
            }
        }
        _ => panic!("expected Switch terminator"),
    }

    let mut saw_binding = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::CopyScalar { dst, src } = stmt {
                if dst.base() == x_local {
                    if let Rvalue::Use(Operand::Copy(place)) = src {
                        if place.base() == c_local
                            && place.projections()
                                == &[
                                    Projection::Field { index: 1 },
                                    Projection::ByteOffset { offset: 0 },
                                ]
                        {
                            saw_binding = true;
                        }
                    }
                }
            }
        }
    }
    assert!(saw_binding, "expected payload binding for x");
}

#[test]
fn test_lower_struct_update() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn main() -> Point {
            let p = Point { x: 1, y: 2 };
            { p | y: 9 }
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    let p_local = body
        .locals
        .iter()
        .enumerate()
        .find(|(_, local)| local.name.as_deref() == Some("p"))
        .map(|(idx, _)| LocalId(idx as u32))
        .expect("p local not found");

    let entry = body.entry;
    let entry_block = &body.blocks[entry.index()];
    let mut saw_copy = false;
    let mut saw_update = false;

    for stmt in &entry_block.stmts {
        match stmt {
            Statement::CopyAggregate { dst, src } => {
                if dst.base() == LocalId(0) && src.base() == p_local {
                    saw_copy = true;
                }
            }
            Statement::CopyScalar { dst, src } => {
                if dst.base() == LocalId(0)
                    && dst.projections() == &[Projection::Field { index: 1 }]
                {
                    if let Rvalue::Use(Operand::Const(Const::Int { value, .. })) = src {
                        if *value == 9 {
                            saw_update = true;
                        }
                    }
                }
            }
            _ => {}
        }
    }

    assert!(saw_copy, "expected base aggregate to be copied");
    assert!(saw_update, "expected updated field write to be emitted");
    assert!(matches!(entry_block.terminator, Terminator::Return));
}

#[test]
fn test_lower_for_range_loop() {
    let source = r#"
        fn main() -> u64 {
            for i in 0..3 { i; };
            0
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    assert!(
        body.blocks.len() >= 4,
        "expected loop lowering to create multiple blocks"
    );

    let mut saw_lt = false;
    let mut saw_add = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::CopyScalar { src, .. } = stmt {
                if let Rvalue::BinOp { op, .. } = src {
                    match op {
                        BinOp::Lt => saw_lt = true,
                        BinOp::Add => saw_add = true,
                        _ => {}
                    }
                }
            }
        }
    }

    assert!(saw_lt, "expected loop condition comparison");
    assert!(saw_add, "expected loop increment");
}

#[test]
fn test_lower_for_array_loop() {
    let source = r#"
        fn main() -> u64 {
            let arr = [1, 2, 3];
            var acc = 0;
            for x in arr { acc = acc + x; };
            acc
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func);

    fn place_has_index<K>(place: &Place<K>) -> bool {
        place
            .projections()
            .iter()
            .any(|proj| matches!(proj, Projection::Index { .. }))
    }

    fn place_any_has_index(place: &PlaceAny) -> bool {
        match place {
            PlaceAny::Scalar(p) => place_has_index(p),
            PlaceAny::Aggregate(p) => place_has_index(p),
        }
    }

    fn operand_has_index(op: &Operand) -> bool {
        match op {
            Operand::Copy(place) => place_has_index(place),
            _ => false,
        }
    }

    fn rvalue_has_index(rv: &Rvalue) -> bool {
        match rv {
            Rvalue::Use(op) => operand_has_index(op),
            Rvalue::BinOp { lhs, rhs, .. } => operand_has_index(lhs) || operand_has_index(rhs),
            Rvalue::UnOp { arg, .. } => operand_has_index(arg),
            Rvalue::AddrOf(place) => place_any_has_index(place),
        }
    }

    let mut saw_index = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::CopyScalar { dst, src } => {
                    if place_has_index(dst) || rvalue_has_index(src) {
                        saw_index = true;
                    }
                }
                Statement::CopyAggregate { dst, src } => {
                    if place_has_index(dst) || place_has_index(src) {
                        saw_index = true;
                    }
                }
                Statement::Call { dst, args, .. } => {
                    if place_any_has_index(dst) {
                        saw_index = true;
                    }
                    if args.iter().any(place_any_has_index) {
                        saw_index = true;
                    }
                }
                _ => {}
            }
        }
    }

    assert!(saw_index, "expected indexed array access in loop body");
}
