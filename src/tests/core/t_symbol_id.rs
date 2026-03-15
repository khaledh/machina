use crate::core::api::{ParseModuleOptions, parse_module_with_id_gen_and_options};
use crate::core::ast::NodeIdGen;
use crate::core::ast::TopLevelItem;
use crate::core::capsule::ModulePath;
use crate::core::context::ParsedContext;
use crate::core::resolve::resolve;

use crate::core::context::ResolvedContext;
use crate::core::symbol_id::{SymbolNs, SymbolPath};

fn resolved_with_module_path(source: &str, module_path: &str) -> ResolvedContext {
    let id_gen = NodeIdGen::new();
    let (module, id_gen) = parse_module_with_id_gen_and_options(
        source,
        id_gen,
        ParseModuleOptions {
            experimental_typestate: true,
        },
    )
    .expect("parse should succeed");
    let parsed = ParsedContext::new(module, id_gen).with_module_path(
        ModulePath::new(module_path.split("::").map(|s| s.to_string()).collect()).unwrap(),
    );
    resolve(parsed).expect("resolve should succeed")
}

#[test]
fn symbol_id_table_records_top_level_and_method_paths() {
    let resolved = resolved_with_module_path(
        r#"
type Conn = {}

Conn :: {
    fn ping(self) {}
}

fn run() {}
"#,
        "app::main",
    );

    let type_id = match &resolved.module.top_level_items[0] {
        TopLevelItem::TypeDef(def) => resolved.def_table.lookup_node_def_id(def.id).unwrap(),
        _ => unreachable!(),
    };
    let method_id = match &resolved.module.top_level_items[1] {
        TopLevelItem::MethodBlock(block) => match &block.method_items[0] {
            crate::core::ast::MethodItem::Def(def) => {
                resolved.def_table.lookup_node_def_id(def.id).unwrap()
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    let type_symbol = resolved.symbol_ids.lookup_symbol_id(type_id).unwrap();
    assert_eq!(type_symbol.ns, SymbolNs::Type);
    assert_eq!(type_symbol.path, SymbolPath::from_names(["Conn"]));

    let method_symbol = resolved.symbol_ids.lookup_symbol_id(method_id).unwrap();
    assert_eq!(method_symbol.ns, SymbolNs::Method);
    assert_eq!(method_symbol.path, SymbolPath::from_names(["Conn", "ping"]));
}

#[test]
fn symbol_id_table_disambiguates_overloaded_callables_by_signature_shape() {
    let resolved = resolved_with_module_path(
        r#"
fn println(s: string) {}
fn println(n: u64) {}
"#,
        "std::io",
    );

    let defs = resolved
        .module
        .func_defs()
        .into_iter()
        .map(|def| resolved.def_table.lookup_node_def_id(def.id).unwrap())
        .collect::<Vec<_>>();
    assert_eq!(defs.len(), 2);

    let left = resolved
        .symbol_ids
        .lookup_symbol_id(defs[0])
        .expect("expected left overload symbol");
    let right = resolved
        .symbol_ids
        .lookup_symbol_id(defs[1])
        .expect("expected right overload symbol");

    assert_eq!(left.path, SymbolPath::from_names(["println"]));
    assert_eq!(right.path, SymbolPath::from_names(["println"]));
    assert_ne!(left, right, "overloads should carry distinct symbol ids");
    assert!(left.disambiguator.is_some());
    assert!(right.disambiguator.is_some());
    assert_eq!(
        resolved
            .symbol_ids
            .lookup_local_def_ids(left)
            .expect("expected exact reverse lookup")
            .len(),
        1
    );
}
