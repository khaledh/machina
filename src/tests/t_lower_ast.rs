use super::*;
use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::lower::drop_glue::{DropGlueRegistry, GeneratedDropGlue};
use crate::mcir::abi::RuntimeFn;
use crate::mcir::interner::GlobalInterner;
use crate::mcir::types::{GlobalItem, GlobalPayload, GlobalSection};
use crate::normalize::normalize;
use crate::nrvo::NrvoAnalyzer;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::typeck::type_check;
use std::collections::HashSet;

fn analyze(source: &str) -> AnalyzedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    let elaborated_context = elaborate(sem_checked_context);

    NrvoAnalyzer::new(elaborated_context).analyze()
}

fn lower_body_with_globals(
    ctx: &AnalyzedContext,
    func_def: &FuncDef,
) -> (FuncBody, Vec<GlobalItem>) {
    let mut interner = GlobalInterner::new();
    let mut drop_glue = DropGlueRegistry::new(ctx.def_table.next_def_id());
    let mut lowerer =
        FuncLowerer::new_function(ctx, func_def, &mut interner, &mut drop_glue, false);
    let body = lowerer.lower().expect("Failed to lower function");
    (body, interner.take())
}

fn lower_body_with_drop_glue(
    ctx: &AnalyzedContext,
    func_def: &FuncDef,
) -> (FuncBody, Vec<GeneratedDropGlue>) {
    let mut interner = GlobalInterner::new();
    let mut drop_glue = DropGlueRegistry::new(ctx.def_table.next_def_id());
    let mut lowerer =
        FuncLowerer::new_function(ctx, func_def, &mut interner, &mut drop_glue, false);
    let body = lowerer.lower().expect("Failed to lower function");
    (body, drop_glue.drain())
}

#[test]
fn test_lower_literal_value() {
    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = analyzed.sir_module.func_defs()[0];
    let mut interner = GlobalInterner::new();
    let mut drop_glue = DropGlueRegistry::new(analyzed.def_table.next_def_id());
    let mut lowerer =
        FuncLowerer::new_function(&analyzed, func_def, &mut interner, &mut drop_glue, false);

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
                assert_eq!(*bits, 32);
            }
            _ => panic!("expected cap const"),
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
    let func_def = *analyzed
        .module
        .func_defs()
        .iter()
        .find(|f| f.sig.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
        Statement::Call {
            dst: Some(dst),
            args,
            ..
        } => {
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

    let id_func_def = *analyzed
        .module
        .func_defs()
        .iter()
        .find(|f| f.sig.name == "id")
        .expect("id not found");
    let (body, _) = lower_body_with_globals(&analyzed, id_func_def);

    println!("Lowered body:\n{}", body);
}

#[test]
fn test_lower_heap_alloc_and_free() {
    let source = r#"
        type Point = { x: u64, y: u64 }

        fn main() -> u64 {
            let p = ^Point { x: 1, y: 2 };
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, generated) = lower_body_with_drop_glue(&analyzed, func_def);

    let mut saw_alloc = false;
    let mut saw_drop_call = false;
    let mut saw_free = false;
    let generated_ids: HashSet<_> = generated.iter().map(|entry| entry.def_id).collect();

    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::Call { callee, .. } = stmt {
                match callee {
                    Callee::Runtime(RuntimeFn::Alloc) => saw_alloc = true,
                    Callee::Def(def_id) if generated_ids.contains(def_id) => saw_drop_call = true,
                    _ => {}
                }
            }
        }
    }

    for entry in &generated {
        for block in &entry.body.blocks {
            for stmt in &block.stmts {
                if let Statement::Call {
                    callee: Callee::Runtime(RuntimeFn::Free),
                    ..
                } = stmt
                {
                    saw_free = true;
                }
            }
        }
    }

    assert!(saw_alloc, "Expected runtime alloc call");
    assert!(saw_drop_call, "Expected drop glue call");
    assert!(saw_free, "Expected drop glue to call runtime free");
}

#[test]
fn test_lower_drop_glue_emits_free() {
    let source = r#"
        type Boxed = { value: ^u64 }

        fn main() -> u64 {
            let b = Boxed { value: ^1 };
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let mut interner = GlobalInterner::new();
    let mut drop_glue = DropGlueRegistry::new(analyzed.def_table.next_def_id());
    let mut lowerer =
        FuncLowerer::new_function(&analyzed, func_def, &mut interner, &mut drop_glue, false);
    let body = lowerer.lower().expect("Failed to lower function");
    let generated = drop_glue.drain();

    assert!(
        generated.len() >= 2,
        "expected drop glue for Boxed and ^u64"
    );

    let generated_ids: HashSet<_> = generated.iter().map(|entry| entry.def_id).collect();
    let mut saw_drop_call = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::Call {
                callee: Callee::Def(def_id),
                ..
            } = stmt
            {
                if generated_ids.contains(def_id) {
                    saw_drop_call = true;
                }
            }
        }
    }
    assert!(saw_drop_call, "expected main to call drop glue");

    let mut saw_free = false;
    for entry in &generated {
        for block in &entry.body.blocks {
            for stmt in &block.stmts {
                if let Statement::Call {
                    callee: Callee::Runtime(RuntimeFn::Free),
                    ..
                } = stmt
                {
                    saw_free = true;
                }
            }
        }
    }
    assert!(saw_free, "expected drop glue to call runtime free");
}

#[test]
fn test_lower_sink_param_dropped() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, generated) = lower_body_with_drop_glue(&analyzed, func_def);
    let generated_ids: HashSet<_> = generated.iter().map(|entry| entry.def_id).collect();

    let mut saw_drop_call = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::Call {
                callee: Callee::Def(def_id),
                ..
            } = stmt
            {
                if generated_ids.contains(def_id) {
                    saw_drop_call = true;
                }
            }
        }
    }

    assert!(saw_drop_call, "expected drop glue call for sink param");
}

#[test]
fn test_lower_sink_call_skips_caller_drop() {
    let source = r#"
        fn consume(sink p: ^u64) -> u64 {
            0
        }

        fn main() -> u64 {
            let p = ^1;
            consume(move p);
            0
        }
    "#;

    let analyzed = analyze(source);
    let funcs = analyzed.sir_module.func_defs();
    let func_def = funcs
        .iter()
        .find(|f| f.sig.name == "main")
        .copied()
        .expect("main not found");
    let (body, generated) = lower_body_with_drop_glue(&analyzed, func_def);
    let generated_ids: HashSet<_> = generated.iter().map(|entry| entry.def_id).collect();

    let mut drop_calls = 0;
    let mut saw_flag_clear = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::Call {
                    callee: Callee::Def(def_id),
                    ..
                } if generated_ids.contains(def_id) => {
                    drop_calls += 1;
                }
                Statement::CopyScalar {
                    src: Rvalue::Use(Operand::Const(Const::Bool(false))),
                    ..
                } => {
                    saw_flag_clear = true;
                }
                _ => {}
            }
        }
    }

    assert_eq!(drop_calls, 1, "expected one drop glue call");
    assert!(
        saw_flag_clear,
        "expected drop flag to be cleared after sink move"
    );
}

#[test]
fn test_lower_heap_implicit_move_skips_double_free() {
    let source = r#"
        fn main() -> u64 {
            let p = ^1;
            let q = p;
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, generated) = lower_body_with_drop_glue(&analyzed, func_def);
    let generated_ids: HashSet<_> = generated.iter().map(|entry| entry.def_id).collect();

    let mut alloc_calls = 0;
    let mut drop_calls = 0;
    let mut saw_flag_clear = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::Call { callee, .. } => match callee {
                    Callee::Runtime(RuntimeFn::Alloc) => alloc_calls += 1,
                    Callee::Def(def_id) if generated_ids.contains(def_id) => drop_calls += 1,
                    _ => {}
                },
                Statement::CopyScalar {
                    src: Rvalue::Use(Operand::Const(Const::Bool(false))),
                    ..
                } => {
                    saw_flag_clear = true;
                }
                _ => {}
            }
        }
    }

    assert_eq!(alloc_calls, 1, "expected one heap allocation");
    assert_eq!(drop_calls, 2, "expected one drop call per binding");
    assert!(
        saw_flag_clear,
        "expected implicit move to clear a drop flag"
    );
}

#[test]
fn test_lower_string_index_emits_memcpy() {
    let source = r#"
        fn main() -> u8 {
            let s = "hi";
            s[1]
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let mut has_memcpy = false;
    let mut has_trap = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::Call { callee, .. } = stmt {
                match callee {
                    Callee::Runtime(RuntimeFn::MemCopy) => has_memcpy = true,
                    Callee::Runtime(RuntimeFn::Trap) => has_trap = true,
                    _ => {}
                }
            }
        }
    }

    assert!(has_memcpy, "expected string index to emit MemCopy");
    assert!(has_trap, "expected bounds check to emit Trap call");
}

#[test]
fn test_lower_tuple_return_literal() {
    let source = r#"
        fn main() -> (u64, bool) {
            (42, true)
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = analyzed
        .module
        .func_defs()
        .iter()
        .find(|f| f.sig.name == "main")
        .copied()
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
    let func_def = *analyzed
        .module
        .func_defs()
        .iter()
        .find(|f| f.sig.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
fn test_lower_var_decl_conditional_drop() {
    let source = r#"
        fn main() -> u64 {
            var p: ^u64;
            p = ^1;
            p = ^2;
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let has_is_initialized = body
        .locals
        .iter()
        .any(|local| local.name.as_deref() == Some("p$is_initialized"));
    assert!(
        has_is_initialized,
        "expected is_initialized local for var decl"
    );

    let has_guard = body
        .blocks
        .iter()
        .any(|bb| matches!(bb.terminator, Terminator::If { .. }));
    assert!(has_guard, "expected conditional drop guarding init flag");
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
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
            for i in 0..3 { i; }
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
            for x in arr { acc = acc + x; }
            acc
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

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
            Rvalue::AddrOf(_) => false,
        }
    }

    let mut saw_index = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::Comment(_) => {}
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
                Statement::MemSet { dst, value, .. } => {
                    if place_has_index(dst) || operand_has_index(value) {
                        saw_index = true;
                    }
                }
                Statement::Call {
                    dst: Some(dst),
                    args,
                    ..
                } => {
                    if place_any_has_index(dst) {
                        saw_index = true;
                    }
                    if args.iter().any(place_any_has_index) {
                        saw_index = true;
                    }
                }
                Statement::Call { dst: None, .. } => {}
            }
        }
    }

    assert!(saw_index, "expected indexed array access in loop body");
}

#[test]
fn test_lower_array_index_emits_bounds_check() {
    let source = r#"
        fn main() -> u64 {
            let arr = [1, 2, 3];
            arr[1]
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let saw_trap_call = body.blocks.iter().any(|block| {
        let has_trap_call = block.stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::Call {
                    callee: Callee::Runtime(RuntimeFn::Trap),
                    ..
                }
            )
        });
        let is_unreachable = matches!(block.terminator, Terminator::Unreachable);
        has_trap_call && is_unreachable
    });

    assert!(saw_trap_call, "expected bounds check runtime trap call");
}

#[test]
fn test_lower_heap_tuple_field_uses_deref() {
    let source = r#"
        fn main() -> u64 {
            let t = ^(1, 2);
            t.0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    fn place_has_deref<K>(place: &Place<K>) -> bool {
        place
            .projections()
            .iter()
            .any(|proj| matches!(proj, Projection::Deref))
    }

    fn place_any_has_deref(place: &PlaceAny) -> bool {
        match place {
            PlaceAny::Scalar(p) => place_has_deref(p),
            PlaceAny::Aggregate(p) => place_has_deref(p),
        }
    }

    fn operand_has_deref(op: &Operand) -> bool {
        match op {
            Operand::Copy(p) | Operand::Move(p) => place_has_deref(p),
            Operand::Const(_) => false,
        }
    }

    fn rvalue_has_deref(rv: &Rvalue) -> bool {
        match rv {
            Rvalue::Use(op) => operand_has_deref(op),
            Rvalue::BinOp { lhs, rhs, .. } => operand_has_deref(lhs) || operand_has_deref(rhs),
            Rvalue::UnOp { arg, .. } => operand_has_deref(arg),
            Rvalue::AddrOf(_) => false,
        }
    }

    let mut saw_deref = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            match stmt {
                Statement::Comment(_) => {}
                Statement::CopyScalar { dst, src } => {
                    if place_has_deref(dst) || rvalue_has_deref(src) {
                        saw_deref = true;
                    }
                }
                Statement::CopyAggregate { dst, src } => {
                    if place_has_deref(dst) || place_has_deref(src) {
                        saw_deref = true;
                    }
                }
                Statement::MemSet { dst, value, .. } => {
                    if place_has_deref(dst) || operand_has_deref(value) {
                        saw_deref = true;
                    }
                }
                Statement::Call { dst, args, .. } => {
                    if dst.as_ref().is_some_and(place_any_has_deref) {
                        saw_deref = true;
                    }
                    if args.iter().any(place_any_has_deref) {
                        saw_deref = true;
                    }
                }
            }
        }
    }

    assert!(
        saw_deref,
        "expected deref projection for heap tuple field access"
    );
}

#[test]
fn test_lower_u8_repeat_literal_emits_memset() {
    let source = r#"
        fn main() -> u64 {
            let buf = u8[0; 8];
            0
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let mut saw_memset = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::MemSet { value, len, .. } = stmt {
                if *len == 8 {
                    if let Operand::Const(Const::Int { value, .. }) = value {
                        if *value == 0 {
                            saw_memset = true;
                        }
                    }
                }
            }
        }
    }

    assert!(saw_memset, "expected MemSet for u8 repeat literal");
}

#[test]
fn test_lower_logical_and_short_circuits() {
    let source = r#"
        fn side() -> bool {
            true
        }

        fn main() -> bool {
            false && side()
        }
    "#;

    let analyzed = analyze(source);
    let funcs = analyzed.sir_module.func_defs();
    let func_def = funcs
        .iter()
        .find(|f| f.sig.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let if_terms: Vec<_> = body
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            Terminator::If {
                then_bb, else_bb, ..
            } => Some((then_bb, else_bb)),
            _ => None,
        })
        .collect();
    assert_eq!(if_terms.len(), 1, "expected one short-circuit if");

    let (then_bb, else_bb) = if_terms[0];
    let then_has_call = body.blocks[then_bb.index()].stmts.iter().any(|stmt| {
        matches!(
            stmt,
            Statement::Call {
                callee: Callee::Def(_),
                ..
            }
        )
    });
    let else_has_call = body.blocks[else_bb.index()].stmts.iter().any(|stmt| {
        matches!(
            stmt,
            Statement::Call {
                callee: Callee::Def(_),
                ..
            }
        )
    });

    assert!(then_has_call, "expected call in then branch for &&");
    assert!(!else_has_call, "expected no call in else branch for &&");
}

#[test]
fn test_lower_logical_or_short_circuits() {
    let source = r#"
        fn side() -> bool {
            true
        }

        fn main() -> bool {
            true || side()
        }
    "#;

    let analyzed = analyze(source);
    let funcs = analyzed.sir_module.func_defs();
    let func_def = funcs
        .iter()
        .find(|f| f.sig.name == "main")
        .expect("main not found");
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let if_terms: Vec<_> = body
        .blocks
        .iter()
        .filter_map(|block| match block.terminator {
            Terminator::If {
                then_bb, else_bb, ..
            } => Some((then_bb, else_bb)),
            _ => None,
        })
        .collect();
    assert_eq!(if_terms.len(), 1, "expected one short-circuit if");

    let (then_bb, else_bb) = if_terms[0];
    let then_has_call = body.blocks[then_bb.index()].stmts.iter().any(|stmt| {
        matches!(
            stmt,
            Statement::Call {
                callee: Callee::Def(_),
                ..
            }
        )
    });
    let else_has_call = body.blocks[else_bb.index()].stmts.iter().any(|stmt| {
        matches!(
            stmt,
            Statement::Call {
                callee: Callee::Def(_),
                ..
            }
        )
    });

    assert!(!then_has_call, "expected no call in then branch for ||");
    assert!(else_has_call, "expected call in else branch for ||");
}

#[test]
fn test_lower_mod_emits_div_mul_sub() {
    let source = r#"
        fn main() -> u64 {
            let x = 10 % 3;
            x
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let mut saw_div = false;
    let mut saw_mul = false;
    let mut saw_sub = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::CopyScalar { src, .. } = stmt {
                if let Rvalue::BinOp { op, .. } = src {
                    match op {
                        BinOp::Div => saw_div = true,
                        BinOp::Mul => saw_mul = true,
                        BinOp::Sub => saw_sub = true,
                        _ => {}
                    }
                }
            }
        }
    }

    assert!(saw_div, "expected div op in modulo lowering");
    assert!(saw_mul, "expected mul op in modulo lowering");
    assert!(saw_sub, "expected sub op in modulo lowering");
}

#[test]
fn test_lower_fstring_calls_runtime() {
    let source = r#"
        fn main() -> string {
            let x: u64 = 42;
            f"val = {x}!"
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, globals) = lower_body_with_globals(&analyzed, func_def);

    let mut saw_fmt_init = false;
    let mut saw_append_bytes = false;
    let mut saw_append_u64 = false;
    let mut saw_fmt_finish = false;
    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::Call { callee, .. } = stmt {
                match callee {
                    Callee::Runtime(RuntimeFn::FmtInit) => saw_fmt_init = true,
                    Callee::Runtime(RuntimeFn::FmtAppendBytes) => saw_append_bytes = true,
                    Callee::Runtime(RuntimeFn::FmtAppendU64) => saw_append_u64 = true,
                    Callee::Runtime(RuntimeFn::FmtFinish) => saw_fmt_finish = true,
                    _ => {}
                }
            }
        }
    }

    assert!(saw_fmt_init, "expected __mc_fmt_init call");
    assert!(saw_append_bytes, "expected __mc_fmt_append_bytes call");
    assert!(saw_append_u64, "expected __mc_fmt_append_u64 call");
    assert!(saw_fmt_finish, "expected __mc_fmt_finish call");

    let mut strings = Vec::new();
    for global in &globals {
        if let GlobalPayload::String(s) = &global.payload {
            strings.push(s.as_str());
        }
    }
    assert!(strings.contains(&"val = "), "missing literal segment");
    assert!(strings.contains(&"!"), "missing trailing literal");
}

#[test]
fn test_lower_fstring_signed_uses_i64_runtime() {
    let source = r#"
        fn main() -> string {
            let x: i64 = -5;
            f"{x}"
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, globals) = lower_body_with_globals(&analyzed, func_def);

    let saw_append_i64 = body.blocks.iter().any(|block| {
        block.stmts.iter().any(|stmt| {
            matches!(
                stmt,
                Statement::Call {
                    callee: Callee::Runtime(RuntimeFn::FmtAppendI64),
                    ..
                }
            )
        })
    });
    assert!(saw_append_i64, "expected __mc_fmt_append_i64 call");

    let saw_minus = globals
        .iter()
        .any(|global| matches!(&global.payload, GlobalPayload::String(s) if s == "-"));
    assert!(
        !saw_minus,
        "did not expect '-' literal for signed formatting"
    );
}

#[test]
fn test_lower_bitwise_ops() {
    let source = r#"
        fn main() -> u64 {
            let a = 1 & 2;
            let b = 1 | 2;
            let c = 1 ^ 2;
            let d = 1 << 3;
            let e = 8 >> 1;
            let f = ~1;
            a + b + c + d + e + f
        }
    "#;

    let analyzed = analyze(source);
    let func_def = analyzed.sir_module.func_defs()[0];
    let (body, _) = lower_body_with_globals(&analyzed, func_def);

    let mut saw_and = false;
    let mut saw_or = false;
    let mut saw_xor = false;
    let mut saw_shl = false;
    let mut saw_shr = false;
    let mut saw_not = false;

    for block in &body.blocks {
        for stmt in &block.stmts {
            if let Statement::CopyScalar { src, .. } = stmt {
                match src {
                    Rvalue::BinOp { op, .. } => match op {
                        BinOp::BitAnd => saw_and = true,
                        BinOp::BitOr => saw_or = true,
                        BinOp::BitXor => saw_xor = true,
                        BinOp::Shl => saw_shl = true,
                        BinOp::Shr => saw_shr = true,
                        _ => {}
                    },
                    Rvalue::UnOp { op, .. } => {
                        if *op == UnOp::BitNot {
                            saw_not = true;
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    assert!(saw_and, "expected bitwise and op");
    assert!(saw_or, "expected bitwise or op");
    assert!(saw_xor, "expected bitwise xor op");
    assert!(saw_shl, "expected shift left op");
    assert!(saw_shr, "expected shift right op");
    assert!(saw_not, "expected bitwise not op");
}
