use crate::context::{HirContext, ResolvedContext};

/// Desugar high-level syntax into core constructs.
///
/// This is a no-op scaffold for now; it will host closure/method sugar later.
pub fn desugar(ctx: ResolvedContext) -> HirContext {
    let ResolvedContext {
        module,
        def_map,
        symbols,
    } = ctx;
    HirContext {
        module,
        def_map,
        symbols,
    }
}

#[cfg(test)]
#[path = "../tests/t_desugar.rs"]
mod tests;
