//! SSA lowering context (type lowering + cache).

use std::collections::HashMap;

use crate::ssa::model::ir::{TypeId, TypeKind, TypeTable};
use crate::tree::semantic as sem;
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId as TcTypeId};

pub(super) struct LowerCtx<'a> {
    type_map: &'a TypeMap,
    pub(super) types: TypeTable,
    type_cache: HashMap<TcTypeId, TypeId>,
}

impl<'a> LowerCtx<'a> {
    pub(super) fn new(type_map: &'a TypeMap) -> Self {
        Self {
            type_map,
            types: TypeTable::new(),
            type_cache: HashMap::new(),
        }
    }

    pub(super) fn ssa_type_for_expr(&mut self, expr: &sem::ValueExpr) -> TypeId {
        self.ssa_type_for_type_id(expr.ty)
    }

    pub(super) fn ssa_type_for_type_id(&mut self, ty_id: TcTypeId) -> TypeId {
        if let Some(id) = self.type_cache.get(&ty_id) {
            return *id;
        }

        let ty = self.type_map.type_table().get(ty_id);
        let id = match ty {
            Type::Unit => self.types.add(TypeKind::Unit),
            Type::Bool => self.types.add(TypeKind::Bool),
            Type::Int { signed, bits } => self.types.add(TypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),
            other => panic!("ssa lower_func unsupported type {:?}", other),
        };

        self.type_cache.insert(ty_id, id);
        id
    }

    pub(super) fn int_info_for_type_id(&self, ty_id: TcTypeId) -> (bool, u8) {
        match self.type_map.type_table().get(ty_id) {
            Type::Int { signed, bits } => (*signed, *bits),
            other => panic!("ssa lower_func expected int type, found {:?}", other),
        }
    }
}
