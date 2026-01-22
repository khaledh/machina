//! Type lowering from semantic types to SSA IR types.

use crate::ssa::{IrStructField, IrTypeCache, IrTypeId, IrTypeKind};
use crate::typeck::type_map::TypeMap;
use crate::types::{Type, TypeId};
use std::collections::HashMap;

/// Lowers type-checker types to SSA IR types with caching.
///
/// Converts semantic types into SSA IR types, caching results to avoid
/// redundant allocations. Maintains two caches:
/// - `type_cache`: Maps type-checker type IDs to SSA type IDs
/// - `type_cache_by_kind`: Maps type structures to SSA type IDs (for structural dedup)
pub(super) struct TypeLowerer<'a> {
    type_map: &'a TypeMap,
    pub(super) ir_type_cache: IrTypeCache,
    by_type_id: HashMap<TypeId, IrTypeId>,
    by_type: HashMap<Type, IrTypeId>,
}

impl<'a> TypeLowerer<'a> {
    pub(super) fn new(type_map: &'a TypeMap) -> Self {
        Self {
            type_map,
            ir_type_cache: IrTypeCache::new(),
            by_type_id: HashMap::new(),
            by_type: HashMap::new(),
        }
    }

    /// Converts a type-checker type ID to an SSA type ID, caching the result.
    pub(super) fn lower_type_id(&mut self, ty_id: TypeId) -> IrTypeId {
        // Check cache first to avoid redundant type creation.
        if let Some(id) = self.by_type_id.get(&ty_id) {
            return *id;
        }

        // Look up the type structure and convert it.
        let ty = self.type_map.type_table().get(ty_id);
        let id = self.lower_type(ty);

        // Cache the mapping for future lookups.
        self.by_type_id.insert(ty_id, id);
        id
    }

    /// Converts a type-checker Type to an SSA TypeId.
    ///
    /// Handles primitive types directly, and recursively converts compound types
    /// (tuples, arrays, structs). Pointer types (Heap, Ref) become SSA Ptr types.
    pub(super) fn lower_type(&mut self, ty: &Type) -> IrTypeId {
        // Check structural cache to deduplicate identical type structures.
        if let Some(id) = self.by_type.get(ty) {
            return *id;
        }

        let id = match ty {
            // Primitive types map directly to their SSA equivalents.
            Type::Unit => self.ir_type_cache.add(IrTypeKind::Unit),
            Type::Bool => self.ir_type_cache.add(IrTypeKind::Bool),
            Type::Int { signed, bits } => self.ir_type_cache.add(IrTypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),

            // String is lowered as a struct with pointer, length, and capacity.
            Type::String => {
                let byte = self.lower_type(&Type::Int {
                    signed: false,
                    bits: 8,
                });
                let ptr = self.ir_type_cache.add(IrTypeKind::Ptr { elem: byte });
                let u32 = self.lower_type(&Type::Int {
                    signed: false,
                    bits: 32,
                });
                let fields = vec![
                    IrStructField {
                        name: "ptr".to_string(),
                        ty: ptr,
                    },
                    IrStructField {
                        name: "len".to_string(),
                        ty: u32,
                    },
                    IrStructField {
                        name: "cap".to_string(),
                        ty: u32,
                    },
                ];
                self.ir_type_cache
                    .add_named(IrTypeKind::Struct { fields }, "string".to_string())
            }

            // Compound types: recursively convert element/field types.
            Type::Tuple { field_tys } => {
                let fields = field_tys
                    .iter()
                    .map(|field| self.lower_type(field))
                    .collect();
                self.ir_type_cache.add(IrTypeKind::Tuple { fields })
            }
            Type::Array { elem_ty, dims } => {
                let elem = self.lower_type(elem_ty);
                let dims = dims.iter().map(|dim| *dim as u64).collect();
                self.ir_type_cache.add(IrTypeKind::Array { elem, dims })
            }
            Type::Struct { name, fields } => {
                let fields = fields
                    .iter()
                    .map(|field| IrStructField {
                        name: field.name.clone(),
                        ty: self.lower_type(&field.ty),
                    })
                    .collect();
                self.ir_type_cache
                    .add_named(IrTypeKind::Struct { fields }, name.clone())
            }

            // Pointer-like types (heap allocations, references) become SSA pointers.
            Type::Heap { elem_ty }
            | Type::Ref {
                elem_ty,
                mutable: _,
            } => {
                let elem = self.lower_type(elem_ty);
                self.ir_type_cache.add(IrTypeKind::Ptr { elem })
            }
            other => panic!("ssa type lowering: unsupported type {:?}", other),
        };

        // Cache the result for structural deduplication.
        self.by_type.insert(ty.clone(), id);
        id
    }

    /// Returns signedness and bit-width for an integer type.
    pub(super) fn int_info(&self, ty_id: TypeId) -> (bool, u8) {
        match self.type_map.type_table().get(ty_id) {
            Type::Int { signed, bits } => (*signed, *bits),
            other => panic!("ssa type lowering: expected int type, found {:?}", other),
        }
    }
}
