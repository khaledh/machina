use crate::core::api::{ParseModuleOptions, parse_module_with_id_gen_and_options};
use crate::core::capsule::ModulePath;
use crate::core::context::ParsedContext;
use crate::core::resolve::resolve;
use crate::core::tree::TopLevelItem;

use crate::core::symbol_id::{SymbolId, SymbolNs, SymbolPath};

fn resolved_with_module_path(
    source: &str,
    module_path: &str,
) -> crate::core::context::ResolvedContext {
    let id_gen = crate::core::tree::NodeIdGen::new();
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
protocol Auth {
    role Client;
}

type Conn = {}

Conn :: {
    fn ping(self) {}
}

fn run() {}
"#,
        "app::main",
    );

    let protocol_id = match &resolved.module.top_level_items[0] {
        TopLevelItem::ProtocolDef(def) => resolved.def_table.lookup_node_def_id(def.id).unwrap(),
        _ => unreachable!(),
    };
    let type_id = match &resolved.module.top_level_items[1] {
        TopLevelItem::TypeDef(def) => resolved.def_table.lookup_node_def_id(def.id).unwrap(),
        _ => unreachable!(),
    };
    let method_id = match &resolved.module.top_level_items[2] {
        TopLevelItem::MethodBlock(block) => match &block.method_items[0] {
            crate::core::tree::MethodItem::Def(def) => {
                resolved.def_table.lookup_node_def_id(def.id).unwrap()
            }
            _ => unreachable!(),
        },
        _ => unreachable!(),
    };

    let protocol_symbol = resolved.symbol_ids.lookup_symbol_id(protocol_id).unwrap();
    assert_eq!(protocol_symbol.ns, SymbolNs::Protocol);
    assert_eq!(protocol_symbol.path, SymbolPath::from_names(["Auth"]));

    let type_symbol = resolved.symbol_ids.lookup_symbol_id(type_id).unwrap();
    assert_eq!(type_symbol.ns, SymbolNs::Type);
    assert_eq!(type_symbol.path, SymbolPath::from_names(["Conn"]));

    let method_symbol = resolved.symbol_ids.lookup_symbol_id(method_id).unwrap();
    assert_eq!(method_symbol.ns, SymbolNs::Method);
    assert_eq!(method_symbol.path, SymbolPath::from_names(["Conn", "ping"]));
}

#[test]
fn symbol_id_table_keeps_overloaded_callables_under_shared_base_symbol() {
    let resolved = resolved_with_module_path(
        r#"
fn println(s: string) {}
fn println(n: u64) {}
"#,
        "std::io",
    );

    let base = SymbolId::new(
        ModulePath::new(vec!["std".into(), "io".into()]).unwrap(),
        SymbolPath::from_names(["println"]),
        SymbolNs::Value,
    );
    let defs = resolved
        .symbol_ids
        .lookup_local_def_ids(&base)
        .expect("expected overloaded defs");
    assert_eq!(defs.len(), 2);
}
