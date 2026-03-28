//! SSA lowering error types.

use std::fmt;

#[derive(Clone, Debug)]
pub struct LowerToIrError;

impl fmt::Display for LowerToIrError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ir lowering error")
    }
}

impl std::error::Error for LowerToIrError {}
