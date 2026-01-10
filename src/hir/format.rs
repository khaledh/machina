//! HIR formatting helpers (currently identical to AST formatting).

use super::Module;

pub fn format_module(module: &Module) -> String {
    module.to_string()
}
