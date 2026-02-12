//! Build the typed tree from the resolved tree.
//!
//! This is the typed step: nodes are annotated with TypeIds.

use crate::resolve::DefId;
use crate::tree::NodeId;
use crate::tree::map::TreeMapper;
use crate::tree::resolved as res;
use crate::tree::typed as typ;
use crate::typecheck::type_map::TypeMap;
use crate::types::TypeId;

struct TypedTreeMapper<'a> {
    type_map: &'a TypeMap,
}

impl<'a> TypedTreeMapper<'a> {
    fn node_type_id(&self, node_id: NodeId) -> TypeId {
        self.type_map
            .lookup_node_type_id(node_id)
            .unwrap_or_else(|| panic!("missing type id for node {node_id:?}"))
    }
}

impl<'a> TreeMapper for TypedTreeMapper<'a> {
    type Context = ();
    type InD = DefId;
    type InT = ();
    type OutD = DefId;
    type OutT = TypeId;

    fn map_def_id(
        &mut self,
        _node_id: NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        *def_id
    }

    fn map_type_payload(
        &mut self,
        node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        self.node_type_id(node_id)
    }
}

pub fn build_module(type_map: &TypeMap, module: &res::Module) -> typ::Module {
    let mut ctx = ();
    let mut mapper = TypedTreeMapper { type_map };
    mapper.map_module(module, &mut ctx)
}
