//! Build the normalized tree from the typed tree.
//!
//! This is the first normalize step: it is a 1:1 mapping of the typed tree,
//! leaving coercions to the normalize pass.

use crate::core::resolve::DefId;
use crate::core::tree::map::TreeMapper;
use crate::core::tree::normalized as norm;
use crate::core::tree::typed as typ;
use crate::core::types::TypeId;

struct NormalizedTreeMapper;

impl TreeMapper for NormalizedTreeMapper {
    type Context = ();
    type InD = DefId;
    type InT = TypeId;
    type OutD = DefId;
    type OutT = TypeId;

    fn map_def_id(
        &mut self,
        _node_id: norm::NodeId,
        def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        *def_id
    }

    fn map_type_payload(
        &mut self,
        _node_id: norm::NodeId,
        payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        *payload
    }
}

pub fn build_module(module: &typ::Module) -> norm::Module {
    let mut ctx = ();
    let mut mapper = NormalizedTreeMapper;
    mapper.map_module(module, &mut ctx)
}
