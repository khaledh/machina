//! Build the resolved tree from the parsed tree.

use crate::resolve::DefId;
use crate::resolve::def_table::NodeDefLookup;
use crate::tree::NodeId;
use crate::tree::map::TreeMapper;
use crate::tree::parsed as par;
use crate::tree::resolved as res;

struct ResolvedTreeMapper<'a> {
    node_def_lookup: &'a NodeDefLookup,
}

impl<'a> TreeMapper for ResolvedTreeMapper<'a> {
    type Context = ();
    type InD = ();
    type InT = ();
    type OutD = DefId;
    type OutT = ();

    fn map_def_id(
        &mut self,
        node_id: NodeId,
        _def_id: &Self::InD,
        _ctx: &mut Self::Context,
    ) -> Self::OutD {
        self.node_def_lookup
            .lookup_node_def_id(node_id)
            .unwrap_or_else(|| panic!("Missing def for NodeId({})", node_id.0))
    }

    fn map_type_payload(
        &mut self,
        _node_id: NodeId,
        _payload: &Self::InT,
        _ctx: &mut Self::Context,
    ) -> Self::OutT {
        ()
    }
}

pub fn build_module(node_def_lookup: &NodeDefLookup, module: &par::Module) -> res::Module {
    let mut ctx = ();
    let mut mapper = ResolvedTreeMapper { node_def_lookup };
    mapper.map_module(module, &mut ctx)
}
