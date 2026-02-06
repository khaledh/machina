use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::typecheck::engine::TypecheckEngine;
use crate::typecheck::errors::TypeCheckError;
use crate::types::Type;

/// Canonical constraint kinds for the rewrite.
#[derive(Debug, Clone, PartialEq, Eq)]
#[allow(dead_code)]
pub(crate) enum Constraint {
    Eq(Type, Type),
    Assignable(Type, Type),
    LetBinding { def_id: DefId, ty: Type },
    NodeType { node_id: NodeId, ty: Type },
}

/// Pass 2: collect typing constraints from AST traversal.
///
/// This is a scaffold pass right now; constraints will be introduced incrementally.
pub(crate) fn run(_engine: &mut TypecheckEngine) -> Result<(), Vec<TypeCheckError>> {
    Ok(())
}
