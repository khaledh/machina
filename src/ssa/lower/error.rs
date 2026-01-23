//! SSA lowering error types.

use crate::diag::Span;

#[derive(Clone, Debug)]
pub enum LoweringErrorKind {
    UnsupportedExpr,
    UnsupportedStmt,
    UnimplementedBranching,
}

#[derive(Clone, Debug)]
pub struct LoweringError {
    pub kind: LoweringErrorKind,
    pub span: Span,
}
