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
    SemanticContext {
        module,
        def_table: ctx.def_table,
        type_map: ctx.type_map,
        symbols: ctx.symbols,
        node_id_gen,
    }
}
