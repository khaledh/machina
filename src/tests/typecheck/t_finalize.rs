use super::*;
use crate::core::context::ParsedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::Parser;
use crate::core::resolve::resolve;
use crate::core::typecheck::{collect, constraints, solver, validate};

fn resolve_source(source: &str) -> crate::core::context::ResolvedContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");
    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();
    let ast_context = ParsedContext::new(module, id_gen);
    resolve(ast_context).expect("Failed to resolve")
}

#[test]
fn test_finalize_materializes_typechecked_context() {
    let source = r#"
        fn test() -> u64 {
            let x = 1;
            x
        }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved, crate::core::resolve::ImportedFacts::default());
    collect::run(&mut engine).expect("collect pass failed");
    constraints::run(&mut engine).expect("constrain pass failed");
    solver::run(&mut engine).expect("solve pass failed");
    validate::run(&mut engine).expect("validate pass failed");
    run(&mut engine).expect("finalize pass failed");

    let checked = materialize(engine).expect("materialize failed");
    let func_body_id = checked.module.func_defs()[0].body.id;
    assert!(checked.type_map.lookup_node_type(func_body_id).is_some());
}

#[test]
fn test_finalize_records_nominal_keys_for_generic_instantiations() {
    let source = r#"
        type Box<T> = { value: T }

        fn main() -> u64 {
            let b: Box<u64> = Box<u64>{ value: 7 };
            b.value
        }
    "#;

    let resolved = resolve_source(source);
    let mut engine = TypecheckEngine::new(resolved, crate::core::resolve::ImportedFacts::default());
    collect::run(&mut engine).expect("collect pass failed");
    constraints::run(&mut engine).expect("constrain pass failed");
    solver::run(&mut engine).expect("solve pass failed");
    validate::run(&mut engine).expect("validate pass failed");
    run(&mut engine).expect("finalize pass failed");

    let checked = materialize(engine).expect("materialize failed");
    let box_def_id = checked
        .module
        .type_defs()
        .into_iter()
        .find(|type_def| type_def.name == "Box")
        .map(|type_def| type_def.def_id)
        .expect("missing Box type def");
    let main_body = &checked
        .module
        .func_defs()
        .into_iter()
        .find(|func| func.sig.name == "main")
        .expect("missing main")
        .body;
    let local_b_def_id = match &main_body.kind {
        crate::core::tree::typed::ExprKind::Block { items, .. } => items
            .iter()
            .find_map(|item| {
                let crate::core::tree::typed::BlockItem::Stmt(stmt) = item else {
                    return None;
                };
                let crate::core::tree::typed::StmtExprKind::LetBind { pattern, .. } = &stmt.kind
                else {
                    return None;
                };
                match &pattern.kind {
                    crate::core::tree::typed::BindPatternKind::Name { ident, def_id }
                        if ident == "b" =>
                    {
                        Some(*def_id)
                    }
                    _ => None,
                }
            })
            .expect("missing local binding for b"),
        _ => panic!("main body is not a block"),
    };
    let local_b = checked
        .def_table
        .lookup_def(local_b_def_id)
        .expect("missing def for local b");
    let box_ty_id = checked
        .type_map
        .lookup_def_type_id(local_b)
        .expect("missing type for local b");
    let key = checked
        .type_map
        .lookup_nominal_key_for_type_id(box_ty_id)
        .expect("missing nominal key for Box<u64>");
    assert_eq!(key.def_id, box_def_id);
    assert_eq!(key.type_args, vec![Type::uint(64)]);
}

#[test]
fn test_infer_type_args_from_instance_matches_generic_nominal_shape() {
    let template = Type::Struct {
        name: "Box<T0>".to_string(),
        fields: vec![crate::core::types::StructField {
            name: "value".to_string(),
            ty: Type::Var(TyVarId::new(0)),
        }],
    };
    let concrete = Type::Struct {
        name: "Box<i32>".to_string(),
        fields: vec![crate::core::types::StructField {
            name: "value".to_string(),
            ty: Type::sint(32),
        }],
    };

    let args = infer_type_args_from_instance(&template, &concrete, 1)
        .expect("expected inferred type args");
    assert_eq!(args, vec![Type::sint(32)]);
}

#[test]
fn test_infer_type_args_from_instance_allows_var_bindings() {
    let template = Type::Enum {
        name: "Option<T0>".to_string(),
        variants: vec![
            crate::core::types::EnumVariant {
                name: "None".to_string(),
                payload: Vec::new(),
            },
            crate::core::types::EnumVariant {
                name: "Some".to_string(),
                payload: vec![Type::Var(TyVarId::new(0))],
            },
        ],
    };
    let concrete = Type::Enum {
        name: "Option<T42>".to_string(),
        variants: vec![
            crate::core::types::EnumVariant {
                name: "None".to_string(),
                payload: Vec::new(),
            },
            crate::core::types::EnumVariant {
                name: "Some".to_string(),
                payload: vec![Type::Var(TyVarId::new(42))],
            },
        ],
    };

    let args = infer_type_args_from_instance(&template, &concrete, 1)
        .expect("expected inferred var type args");
    assert_eq!(args, vec![Type::Var(TyVarId::new(42))]);
}
