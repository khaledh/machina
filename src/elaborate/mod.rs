use crate::context::{SemanticCheckedContext, SemanticContext};
use crate::tree::semantic as sem;
mod calls;
mod closure;
mod elaborator;
mod place;
mod types;
mod value;

use crate::elaborate::elaborator::Elaborator;

/// Elaborate a normalized tree into a semantic tree using semantic
/// analysis results.
///
/// Step 1: insert implicit move nodes based on semck results.
pub fn elaborate(ctx: SemanticCheckedContext) -> SemanticContext {
    let SemanticCheckedContext {
        module,
        def_table,
        type_map,
        call_sigs,
        symbols,
        node_id_gen,
        implicit_moves,
        init_assigns,
        full_init_assigns,
        closure_captures,
    } = ctx;
    let mut node_id_gen = node_id_gen;
    let mut def_table = def_table;
    let mut type_map = type_map;
    let mut elaborator = Elaborator::new(
        &mut def_table,
        &mut type_map,
        &call_sigs,
        &mut node_id_gen,
        &implicit_moves,
        &init_assigns,
        &full_init_assigns,
        &closure_captures,
    );

    let module = elaborator.elaborate_module(&module);

    // Generate method names for lifted closures and add them to the symbol table
    let mut symbols = symbols;
    let mut used_names: std::collections::HashSet<String> =
        symbols.def_names.values().cloned().collect();
    for method_block in module.method_blocks() {
        let type_name = method_block.type_name.as_str();
        for method_item in &method_block.method_items {
            let method_def = match method_item {
                sem::MethodItem::Def(method_def) => method_def,
                sem::MethodItem::Decl(_) => continue,
            };
            if symbols.def_names.contains_key(&method_def.def_id) {
                continue;
            }
            let base_name = format!("{type_name}${}", method_def.sig.name);
            let name = if used_names.contains(&base_name) {
                format!("{base_name}${}", method_def.def_id.0)
            } else {
                base_name
            };
            used_names.insert(name.clone());
            symbols.register_generated_def(method_def.def_id, name);
        }
    }

    SemanticContext {
        module,
        def_table,
        type_map,
        symbols,
        node_id_gen,
    }
}
