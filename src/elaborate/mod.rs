use crate::context::{SemanticCheckedContext, SemanticContext};
mod elaborator;

use crate::elaborate::elaborator::Elaborator;

/// Elaborate a normalized tree into a semantic tree using semantic
/// analysis results.
///
/// Step 1: insert implicit move nodes based on semck results.
pub fn elaborate(ctx: SemanticCheckedContext) -> SemanticContext {
    let mut node_id_gen = ctx.node_id_gen;
    let mut def_table = ctx.def_table;
    let mut type_map = ctx.type_map;
    let mut elaborator = Elaborator::new(
        &mut def_table,
        &mut type_map,
        &mut node_id_gen,
        &ctx.implicit_moves,
        &ctx.init_assigns,
        &ctx.full_init_assigns,
    );

    let module = elaborator.elaborate_module(&ctx.module);

    // Generate method names for lifted closures and add them to the symbol table
    let mut symbols = ctx.symbols;
    let mut used_names: std::collections::HashSet<String> =
        symbols.def_names.values().cloned().collect();
    for method_block in module.method_blocks() {
        let type_name = method_block.type_name.as_str();
        for method_def in &method_block.method_defs {
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
