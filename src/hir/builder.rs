//! Build HIR from resolved AST.
//!
//! For now this is a no-op conversion. As we make HIR semantically explicit,
//! this will be the place to desugar and normalize AST constructs.

use crate::ast;

pub fn build(module: ast::Module) -> crate::hir::Module {
    module
}
