use crate::context::{ElaboratedContext, SemanticCheckedContext};
mod elaborator;

use crate::elaborate::elaborator::Elaborator;

/// Elaborate SIR using semantic analysis results.
///
/// Step 1: insert implicit move nodes based on semck results.
pub fn elaborate(ctx: SemanticCheckedContext) -> ElaboratedContext {
    let mut node_id_gen = ctx.node_id_gen;
    let mut elaborator = Elaborator::new(
        &ctx.type_map,
        &mut node_id_gen,
        &ctx.implicit_moves,
        &ctx.init_assigns,
        &ctx.full_init_assigns,
    );
    let module = elaborator.elaborate_module(&ctx.module);
    ElaboratedContext {
        module,
        def_table: ctx.def_table,
        type_map: ctx.type_map,
        symbols: ctx.symbols,
        node_id_gen,
    }
}
