//! SSA lowering context (type lowering + cache).

use std::collections::HashMap;

use crate::ssa::model::ir::{self, TypeId, TypeKind, TypeTable};
use crate::tree::{NodeId, semantic as sem};
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId as TcTypeId};

pub(super) struct LowerCtx<'a> {
    type_map: &'a TypeMap,
    pub(super) types: TypeTable,
    type_cache: HashMap<TcTypeId, TypeId>,
    type_cache_by_kind: HashMap<Type, TypeId>,
}

impl<'a> LowerCtx<'a> {
    pub(super) fn new(type_map: &'a TypeMap) -> Self {
        Self {
            type_map,
            types: TypeTable::new(),
            type_cache: HashMap::new(),
            type_cache_by_kind: HashMap::new(),
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
        let id = self.ssa_type_for_type(ty);

        self.type_cache.insert(ty_id, id);
        id
    }

    pub(super) fn ssa_type_for_type(&mut self, ty: &Type) -> TypeId {
        if let Some(id) = self.type_cache_by_kind.get(ty) {
            return *id;
        }

        let id = match ty {
            Type::Unit => self.types.add(TypeKind::Unit),
            Type::Bool => self.types.add(TypeKind::Bool),
            Type::Int { signed, bits } => self.types.add(TypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),
            Type::String => {
                // Model string as a named struct for now (ptr + len + cap).
                let byte = self.ssa_type_for_type(&Type::Int {
                    signed: false,
                    bits: 8,
                });
                let ptr = self.types.add(TypeKind::Ptr { elem: byte });
                let u32 = self.ssa_type_for_type(&Type::Int {
                    signed: false,
                    bits: 32,
                });
                let fields = vec![
                    ir::StructField {
                        name: "ptr".to_string(),
                        ty: ptr,
                    },
                    ir::StructField {
                        name: "len".to_string(),
                        ty: u32,
                    },
                    ir::StructField {
                        name: "cap".to_string(),
                        ty: u32,
                    },
                ];
                self.types
                    .add_named(TypeKind::Struct { fields }, "string".to_string())
            }
            Type::Tuple { field_tys } => {
                let fields = field_tys
                    .iter()
                    .map(|field| self.ssa_type_for_type(field))
                    .collect();
                self.types.add(TypeKind::Tuple { fields })
            }
            Type::Array { elem_ty, dims } => {
                let elem = self.ssa_type_for_type(elem_ty);
                let dims = dims.iter().map(|dim| *dim as u64).collect();
                self.types.add(TypeKind::Array { elem, dims })
            }
            Type::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| ir::StructField {
                        name: field.name.clone(),
                        ty: self.ssa_type_for_type(&field.ty),
                    })
                    .collect();
                self.types
                    .add_named(TypeKind::Struct { fields }, name.clone())
            }
            Type::Heap { elem_ty }
            | Type::Ref {
                elem_ty,
                mutable: _,
            } => {
                let elem = self.ssa_type_for_type(elem_ty);
                self.types.add(TypeKind::Ptr { elem })
            }
            other => panic!("ssa lower_func unsupported type {:?}", other),
        };

        self.type_cache_by_kind.insert(ty.clone(), id);
        id
    }

    pub(super) fn int_info_for_type_id(&self, ty_id: TcTypeId) -> (bool, u8) {
        match self.type_map.type_table().get(ty_id) {
            Type::Int { signed, bits } => (*signed, *bits),
            other => panic!("ssa lower_func expected int type, found {:?}", other),
        }
    }

    pub(super) fn call_plan_for(&self, node_id: NodeId) -> Option<sem::CallPlan> {
        self.type_map.lookup_call_plan(node_id)
    }
}
