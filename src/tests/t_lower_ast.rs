use super::*;
use crate::context::AstContext;
use crate::lexer::{LexError, Lexer, Token};
use crate::nrvo::NrvoAnalyzer;
use crate::parser::Parser;
use crate::resolver::resolve;
use crate::type_check::type_check;

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
