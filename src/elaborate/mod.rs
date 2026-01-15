use crate::context::{SemanticCheckedContext, SemanticContext};
mod elaborator;

use crate::elaborate::elaborator::Elaborator;

/// Elaborate a normalized tree into a semantic tree using semantic
/// analysis results.
///
/// Step 1: insert implicit move nodes based on semck results.
pub fn elaborate(ctx: SemanticCheckedContext) -> SemanticContext {
    let mut node_id_gen = ctx.node_id_gen;
    let mut elaborator = Elaborator::new(
        &ctx.type_map,
        &mut node_id_gen,
        &ctx.implicit_moves,
        &ctx.init_assigns,
        &ctx.full_init_assigns,
    );

    let module = elaborator.elaborate_module(&ctx.module);

    // Add lifted closure names (generated) to symbol table
    let mut symbols = ctx.symbols;
    let mut used_names: std::collections::HashSet<String> =
        symbols.def_names.values().cloned().collect();
    for closure_def in module.closure_defs() {
        if symbols.def_names.contains_key(&closure_def.def_id) {
            continue;
        }
        let base_name = ctx
            .def_table
            .lookup_def(closure_def.def_id)
            .map(|def| def.name.clone())
            .unwrap_or_else(|| format!("closure${}", closure_def.def_id.0));
        let name = if used_names.contains(&base_name) {
            format!("{base_name}${}", closure_def.def_id.0)
        } else {
            base_name
        };
        used_names.insert(name.clone());
        symbols.register_generated_def(closure_def.def_id, name);
    }

    SemanticContext {
        module,
        def_table: ctx.def_table,
        type_map: ctx.type_map,
        symbols,
        node_id_gen,
    }
}
