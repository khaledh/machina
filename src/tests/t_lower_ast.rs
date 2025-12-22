use super::*;
use crate::context::AstContext;
use crate::lexer::{LexError, Lexer, Token};
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

#[test]
fn test_lower_literal_value() {
    let source = r#"
        fn main() -> u64 {
            42
        }
    "#;

    let analyzed = analyze(source);
    let func = analyzed.module.funcs()[0];
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, id_func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
    let mut lowerer = FuncLowerer::new(&analyzed, func);

    let body = lowerer.lower().expect("Failed to lower function");

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
