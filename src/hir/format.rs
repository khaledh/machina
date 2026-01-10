//! HIR formatting helpers (currently identical to AST formatting).

use std::fmt;

use crate::hir::model::{BindPattern, Module};

pub fn format_module(module: &Module) -> String {
    format!("{module:?}")
}

impl fmt::Display for BindPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{self:?}")
    }
}
