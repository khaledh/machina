//! SSA lowering context (type lowering + cache).

use std::collections::HashMap;

use crate::ssa::model::ir::{self, TypeId, TypeKind, TypeTable};
use crate::tree::{NodeId, semantic as sem};
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId as TcTypeId};

/// Lowering context that manages type conversion and caching.
///
/// Converts type-checker types into SSA IR types, caching results to avoid
/// redundant allocations. Maintains two caches:
/// - `type_cache`: Maps type-checker type IDs to SSA type IDs
/// - `type_cache_by_kind`: Maps type structures to SSA type IDs (for structural dedup)
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

    /// Converts a type-checker type ID to an SSA type ID, caching the result.
    pub(super) fn ssa_type_for_type_id(&mut self, ty_id: TcTypeId) -> TypeId {
        // Check cache first to avoid redundant type creation.
        if let Some(id) = self.type_cache.get(&ty_id) {
            return *id;
        }

        // Look up the type structure and convert it.
        let ty = self.type_map.type_table().get(ty_id);
        let id = self.ssa_type_for_type(ty);

        // Cache the mapping for future lookups.
        self.type_cache.insert(ty_id, id);
        id
    }

    /// Converts a type-checker Type to an SSA TypeId.
    ///
    /// Handles primitive types directly, and recursively converts compound types
    /// (tuples, arrays, structs). Pointer types (Heap, Ref) become SSA Ptr types.
    pub(super) fn ssa_type_for_type(&mut self, ty: &Type) -> TypeId {
        // Check structural cache to deduplicate identical type structures.
        if let Some(id) = self.type_cache_by_kind.get(ty) {
            return *id;
        }

        let id = match ty {
            // Primitive types map directly to their SSA equivalents.
            Type::Unit => self.types.add(TypeKind::Unit),
            Type::Bool => self.types.add(TypeKind::Bool),
            Type::Int { signed, bits } => self.types.add(TypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),

            // String is lowered as a struct with pointer, length, and capacity.
            Type::String => {
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

            // Compound types: recursively convert element/field types.
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

            // Pointer-like types (heap allocations, references) become SSA pointers.
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

        // Cache the result for structural deduplication.
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
