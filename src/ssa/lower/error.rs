//! SSA lowering error types.

use std::fmt;

#[derive(Clone, Debug)]
pub struct LoweringError;

impl fmt::Display for LoweringError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ssa lowering error")
    }
}

impl std::error::Error for LoweringError {}
