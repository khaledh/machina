use thiserror::Error;

use crate::ast::NodeId;
use crate::lower::lower_ast::PlaceKind;
use crate::resolve::def_map::DefId;

#[derive(Debug, Error)]
pub enum LowerError {
    #[error("expression def not found for node {0:?}")]
    ExprDefNotFound(NodeId),

    #[error("expression type not found for node {0:?}")]
    ExprTypeNotFound(NodeId),

    #[error("expression is not a place for node {0:?}")]
    ExprIsNotPlace(NodeId),

    #[error("expression is not an aggregate for node {0:?}")]
    ExprIsNotAggregate(NodeId),

    #[error("expression not allowed in value context for node {0:?}")]
    ExprNotAllowedInValueContext(NodeId),

    #[error("place is not {expected:?} for node {node_id:?}")]
    PlaceKindMismatch {
        node_id: NodeId,
        expected: PlaceKind,
    },

    #[error("variable local not found for node {0:?} (def {1:?})")]
    VarLocalNotFound(NodeId, DefId),

    #[error("unsupported operand expression for node {0:?}")]
    UnsupportedOperandExpr(NodeId),

    #[error("pattern does not match expected shape for node {0:?}")]
    PatternMismatch(NodeId),

    #[error("unsupported aggregate rhs for node {0:?}")]
    UnsupportedAggregateRhs(NodeId),
}
