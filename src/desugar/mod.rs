use crate::context::{HirContext, ResolvedContext};
use crate::hir::builder::HirBuilder;

/// Desugar high-level syntax into core constructs.
///
/// This is a no-op scaffold for now; it will host closure/method sugar later.
pub fn desugar(ctx: ResolvedContext) -> HirContext {
    let ResolvedContext {
        module,
        def_map,
        symbols,
    } = ctx;
    let hir_builder = HirBuilder::new(&def_map);
    let hir_module = hir_builder.build_module(module.clone());
    HirContext {
        ast_module: module,
        module: hir_module,
        def_map,
        symbols,
    }
}

#[cfg(test)]
#[path = "../tests/t_desugar.rs"]
mod tests;
