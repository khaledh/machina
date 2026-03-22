use crate::core::ast::{FuncDef, MethodItem, Module, TopLevelItem, TypeExprKind};
use crate::core::context::ParsedContext;
use crate::core::context::ResolvedContext;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::monomorphize::{monomorphize_resolved, monomorphize_resolved_with_stats};
use crate::core::parse::Parser;
use crate::core::resolve::DefTable;
use crate::core::resolve::resolve;
use crate::core::typecheck::type_check;

fn resolve_context(source: &str) -> (ResolvedContext, DefTable) {
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
    let def_table = resolved_context.def_table.clone();
    (resolved_context, def_table)
}

fn find_func_def<'a>(module: &'a Module, def_table: &DefTable, name: &str) -> Option<&'a FuncDef> {
    module.top_level_items.iter().find_map(|item| {
        if let TopLevelItem::FuncDef(func_def) = item {
            let def_name = def_table
                .lookup_def(def_table.def_id(func_def.id))
                .map(|def| def.name.as_str());
            if def_name == Some(name) {
                return Some(func_def);
            }
        }
        None
    })
}

fn count_method_defs(module: &Module, method_name: &str) -> usize {
    module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::MethodBlock(block) => Some(block),
            _ => None,
        })
        .flat_map(|block| block.method_items.iter())
        .filter(|item| match item {
            MethodItem::Def(def) => def.sig.name == method_name,
            MethodItem::Decl(decl) => decl.sig.name == method_name,
        })
        .count()
}

#[test]
fn test_monomorphize_allows_multiple_instantiations() {
    let source = r#"
        fn id<T>(x: T) -> T { x }

        fn test() -> u64 {
            let a = id(1);
            let b = id(true);
            if b { a } else { a }
        }
    "#;

    let (resolved_context, _def_table) = resolve_context(source);
    let type_checked = type_check(resolved_context.clone()).expect("type check failed");
    let monomorphized = monomorphize_resolved(resolved_context, &type_checked.generic_insts)
        .expect("monomorphize failed");

    let id_count = monomorphized
        .module
        .top_level_items
        .iter()
        .filter(|item| {
            if let TopLevelItem::FuncDef(func_def) = item {
                let name = monomorphized
                    .def_table
                    .lookup_def(monomorphized.def_table.def_id(func_def.id))
                    .map(|def| def.name.as_str());
                return name == Some("id");
            }
            false
        })
        .count();
    assert_eq!(id_count, 2, "expected two monomorphized id definitions");
}

#[test]
fn test_monomorphize_strips_type_params_for_single_inst() {
    let source = r#"
        fn id<T>(x: T) -> T { x }

        fn test() -> u64 {
            id(1)
        }
    "#;

    let (resolved_context, _def_table) = resolve_context(source);
    let type_checked = type_check(resolved_context.clone()).expect("type check failed");
    let monomorphized = monomorphize_resolved(resolved_context, &type_checked.generic_insts)
        .expect("monomorphize failed");

    let func_def = find_func_def(&monomorphized.module, &monomorphized.def_table, "id")
        .expect("expected monomorphized id function");
    assert!(
        func_def.sig.type_params.is_empty(),
        "type params should be stripped after monomorphization"
    );
}

#[test]
fn test_monomorphize_generic_methods_multiple_instantiations() {
    let source = r#"
        type Boxed = { value: u64 }

        Boxed::{
            fn cast<T>(self, x: T) -> T { x }
        }

        fn test() -> u64 {
            let b1 = Boxed { value: 1 };
            let b2 = Boxed { value: 2 };
            let a = b1.cast(1);
            let b = b2.cast(true);
            if b { a } else { a }
        }
    "#;

    let (resolved_context, _def_table) = resolve_context(source);
    let type_checked = type_check(resolved_context.clone()).expect("type check failed");
    let monomorphized = monomorphize_resolved(resolved_context, &type_checked.generic_insts)
        .expect("monomorphize failed");

    let count = count_method_defs(&monomorphized.module, "cast");
    assert_eq!(count, 2, "expected two monomorphized cast methods");
}

#[test]
fn test_monomorphize_specializes_generic_receiver_method_block_type_args() {
    let source = r#"
        type Box<T> = { value: T }

        Box<T>::{
            fn value_of(self) -> T { self.value }
        }

        fn test() -> u64 {
            let boxed = Box<u64> { value: 7 };
            boxed.value_of()
        }
    "#;

    let (resolved_context, _def_table) = resolve_context(source);
    let type_checked = type_check(resolved_context.clone()).expect("type check failed");
    let monomorphized = monomorphize_resolved(resolved_context, &type_checked.generic_insts)
        .expect("monomorphize failed");

    let blocks = monomorphized
        .module
        .top_level_items
        .iter()
        .filter_map(|item| match item {
            TopLevelItem::MethodBlock(block) if block.type_name == "Box" => Some(block),
            _ => None,
        })
        .collect::<Vec<_>>();
    assert_eq!(blocks.len(), 1, "expected one specialized Box method block");
    assert_eq!(
        blocks[0].type_args.len(),
        1,
        "expected one receiver type arg"
    );
    match &blocks[0].type_args[0].kind {
        TypeExprKind::Named { ident, type_args } => {
            assert_eq!(ident, "u64");
            assert!(type_args.is_empty(), "expected concrete receiver type arg");
        }
        other => panic!("expected named receiver type arg, got {other:?}"),
    }
}

#[test]
fn test_monomorphize_reuses_duplicate_instantiation_requests() {
    let source = r#"
        fn id<T>(x: T) -> T { x }

        fn test() -> u64 {
            let a = id(1);
            let b = id(2);
            a + b
        }
    "#;

    let (resolved_context, _def_table) = resolve_context(source);
    let type_checked = type_check(resolved_context.clone()).expect("type check failed");
    let (monomorphized, stats) =
        monomorphize_resolved_with_stats(resolved_context, &type_checked.generic_insts)
            .expect("monomorphize failed");

    let id_count = monomorphized
        .module
        .top_level_items
        .iter()
        .filter(|item| {
            if let TopLevelItem::FuncDef(func_def) = item {
                let name = monomorphized
                    .def_table
                    .lookup_def(monomorphized.def_table.def_id(func_def.id))
                    .map(|def| def.name.as_str());
                return name == Some("id");
            }
            false
        })
        .count();
    assert_eq!(
        id_count, 1,
        "expected one monomorphized id definition for repeated u64 instantiation requests"
    );
    assert_eq!(
        stats.requested_instantiations, 2,
        "expected two call-site requests"
    );
    assert_eq!(
        stats.unique_instantiations, 1,
        "expected one unique instantiation key"
    );
    assert_eq!(stats.reused_requests, 1, "expected one memoized reuse");
}
