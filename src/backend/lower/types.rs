//! Type lowering from semantic types to SSA IR types.

use crate::ir::{IrStructField, IrTypeCache, IrTypeId, IrTypeKind};
use crate::typecheck::type_map::TypeMap;
use crate::types::{FnParamMode, Type, TypeId};
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
    ptr_cache: HashMap<IrTypeId, IrTypeId>,
    enum_layouts: HashMap<TypeId, EnumLayout>,
    fmt_ty: Option<IrTypeId>,
}

impl<'a> TypeLowerer<'a> {
    pub(super) fn new(type_map: &'a TypeMap) -> Self {
        Self {
            type_map,
            ir_type_cache: IrTypeCache::new(),
            by_type_id: HashMap::new(),
            by_type: HashMap::new(),
            ptr_cache: HashMap::new(),
            enum_layouts: HashMap::new(),
            fmt_ty: None,
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
            Type::Int {
                signed,
                bits,
                bounds: _,
                nonzero: _,
            } => self.ir_type_cache.add(IrTypeKind::Int {
                signed: *signed,
                bits: *bits,
            }),
            Type::Char => self.ir_type_cache.add(IrTypeKind::Int {
                signed: false,
                bits: 32,
            }),
            Type::Range { elem_ty } => self.lower_type(elem_ty),
            Type::Fn { params, ret_ty } => {
                let params = params
                    .iter()
                    .map(|param| {
                        let param_ty = self.lower_type(&param.ty);
                        match param.mode {
                            FnParamMode::In | FnParamMode::Sink => {
                                if param.ty.is_scalar() {
                                    param_ty
                                } else {
                                    self.ptr_to(param_ty)
                                }
                            }
                            FnParamMode::Out | FnParamMode::InOut => self.ptr_to(param_ty),
                        }
                    })
                    .collect();
                let ret = self.lower_type(ret_ty);
                self.ir_type_cache.add(IrTypeKind::Fn { params, ret })
            }

            // String is lowered as a struct with pointer, length, and capacity.
            Type::String => {
                let byte = self.lower_type(&Type::Int {
                    signed: false,
                    bits: 8,
                    bounds: None,
                    nonzero: false,
                });
                let ptr = self.ir_type_cache.add(IrTypeKind::Ptr { elem: byte });
                let u32 = self.lower_type(&Type::Int {
                    signed: false,
                    bits: 32,
                    bounds: None,
                    nonzero: false,
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

            // Slice is lowered as a struct { ptr, len } with a typed pointer.
            Type::Slice { elem_ty } => {
                let elem = self.lower_type(elem_ty);
                let ptr = self.ptr_to(elem);
                let u64 = self.lower_type(&Type::uint(64));
                let fields = vec![
                    IrStructField {
                        name: "ptr".to_string(),
                        ty: ptr,
                    },
                    IrStructField {
                        name: "len".to_string(),
                        ty: u64,
                    },
                ];
                self.ir_type_cache.add(IrTypeKind::Struct { fields })
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
                let placeholder = self.ir_type_cache.add_placeholder_named(name.clone());
                self.by_type.insert(ty.clone(), placeholder);

                let fields = fields
                    .iter()
                    .map(|field| IrStructField {
                        name: field.name.clone(),
                        ty: self.lower_type(&field.ty),
                    })
                    .collect();

                self.ir_type_cache
                    .update_kind(placeholder, IrTypeKind::Struct { fields });
                placeholder
            }
            Type::Enum { name, .. } => {
                let ty_id = self.type_map.type_table().lookup_id(ty).unwrap_or_else(|| {
                    panic!("backend type lowering: missing enum type id for {name}")
                });
                let placeholder = self.ir_type_cache.add_placeholder_named(name.clone());
                self.by_type.insert(ty.clone(), placeholder);
                let layout = self.enum_layout(ty_id);
                let fields = vec![
                    IrStructField {
                        name: "tag".to_string(),
                        ty: layout.tag_ty,
                    },
                    IrStructField {
                        name: "payload".to_string(),
                        ty: layout.blob_ty,
                    },
                ];
                self.ir_type_cache
                    .update_kind(placeholder, IrTypeKind::Struct { fields });
                placeholder
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
            other => panic!("backend type lowering: unsupported type {:?}", other),
        };

        // Cache the result for structural deduplication.
        self.by_type.insert(ty.clone(), id);
        id
    }

    /// Returns a pointer type to the given SSA type, caching per element type.
    pub(super) fn ptr_to(&mut self, elem: IrTypeId) -> IrTypeId {
        if let Some(id) = self.ptr_cache.get(&elem) {
            return *id;
        }

        let id = self.ir_type_cache.add(IrTypeKind::Ptr { elem });
        self.ptr_cache.insert(elem, id);
        id
    }

    /// Returns the internal formatter struct type (fmt { ptr, len, cap }).
    pub(super) fn fmt_type(&mut self) -> IrTypeId {
        if let Some(id) = self.fmt_ty {
            return id;
        }

        let u64_ty = self.lower_type(&Type::uint(64));
        let fields = vec![
            IrStructField {
                name: "ptr".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "len".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "cap".to_string(),
                ty: u64_ty,
            },
        ];
        let id = self
            .ir_type_cache
            .add_named(IrTypeKind::Struct { fields }, "fmt".to_string());
        self.fmt_ty = Some(id);
        id
    }

    /// Returns signedness and bit-width for an integer type.
    pub(super) fn int_info(&self, ty_id: TypeId) -> (bool, u8) {
        match self.type_map.type_table().get(ty_id) {
            Type::Int { signed, bits, .. } => (*signed, *bits),
            other => panic!(
                "backend type lowering: expected int type, found {:?}",
                other
            ),
        }
    }

    pub(super) fn enum_layout(&mut self, ty_id: TypeId) -> &EnumLayout {
        if self.enum_layouts.contains_key(&ty_id) {
            return self.enum_layouts.get(&ty_id).unwrap();
        }

        let enum_ty = self.type_map.type_table().get(ty_id);
        let Type::Enum { name: _, variants } = enum_ty else {
            panic!(
                "backend type lowering: expected enum type, found {:?}",
                enum_ty
            );
        };

        // Clone variants to avoid holding a borrow while we mutate self
        let variants = variants.clone();

        let tag_ty = self.lower_type(&Type::Int {
            signed: false,
            bits: 32,
            bounds: None,
            nonzero: false,
        });

        let mut max_payload_size = 0u64;
        let mut max_payload_align = 1u64;
        let mut variant_layouts = Vec::new();

        for (i, variant) in variants.iter().enumerate() {
            let tag_value = i as u32;
            if variant.payload.is_empty() {
                // No payload, just a scalar tag.
                variant_layouts.push(EnumVariantLayout {
                    name: variant.name.clone(),
                    tag: tag_value,
                    field_tys: vec![],
                    field_offsets: vec![],
                    payload_size: 0,
                    payload_align: 1,
                });
                continue;
            }
            if variant.payload.len() == 1 {
                // Single payload element, use the element's layout directly.
                let payload_ty = self.lower_type(&variant.payload[0]);
                let payload_layout = self.ir_type_cache.layout(payload_ty);
                let payload_size = payload_layout.size();
                let payload_align = payload_layout.align();
                variant_layouts.push(EnumVariantLayout {
                    name: variant.name.clone(),
                    tag: tag_value,
                    field_tys: vec![payload_ty],
                    field_offsets: vec![0],
                    payload_size,
                    payload_align,
                });
                max_payload_size = max_payload_size.max(payload_size);
                max_payload_align = max_payload_align.max(payload_align);
                continue;
            }

            // Payload size > 1: create a tuple type and use its layout offsets.
            let payload_field_tys: Vec<_> = variant
                .payload
                .iter()
                .map(|field| self.lower_type(field))
                .collect();

            let tuple_ty = self.ir_type_cache.add(IrTypeKind::Tuple {
                fields: payload_field_tys.clone(),
            });
            let payload_layout = self.ir_type_cache.layout(tuple_ty);
            let payload_size = payload_layout.size();
            let payload_align = payload_layout.align();
            let field_offsets = payload_layout.field_offsets().to_vec();

            variant_layouts.push(EnumVariantLayout {
                name: variant.name.clone(),
                tag: tag_value,
                field_tys: payload_field_tys,
                field_offsets,
                payload_size,
                payload_align,
            });
            max_payload_size = max_payload_size.max(payload_size);
            max_payload_align = max_payload_align.max(payload_align);
        }

        let blob_ty = self.ir_type_cache.add(IrTypeKind::Blob {
            size: max_payload_size,
            align: max_payload_align,
        });
        let layout = EnumLayout {
            tag_ty,
            blob_ty,
            variants: variant_layouts,
        };
        self.enum_layouts.insert(ty_id, layout);
        self.enum_layouts.get(&ty_id).unwrap()
    }
}

pub struct EnumLayout {
    pub tag_ty: IrTypeId,  // u32 tag type
    pub blob_ty: IrTypeId, // blob of bytes for the payload
    pub variants: Vec<EnumVariantLayout>,
}

pub struct EnumVariantLayout {
    pub name: String,
    pub tag: u32,                 // index of the variant
    pub field_tys: Vec<IrTypeId>, // lowered field types
    pub field_offsets: Vec<u64>,  // offsets of the fields in the blob
    #[allow(dead_code)]
    pub payload_size: u64,
    #[allow(dead_code)]
    pub payload_align: u64,
}

impl EnumLayout {
    pub(super) fn variant_by_name(&self, name: &str) -> &EnumVariantLayout {
        self.variants
            .iter()
            .find(|variant| variant.name == name)
            .unwrap_or_else(|| panic!("backend enum layout missing variant {name}"))
    }
}
