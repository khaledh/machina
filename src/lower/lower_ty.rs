use std::collections::HashMap;

use crate::mcir::TyKind;
use crate::mcir::types::{StructField, TyId, TyTable};
use crate::types::Type;

#[derive(Debug)]
pub struct TyLowerer {
    pub table: TyTable,
    map: HashMap<Type, TyId>,
}

impl TyLowerer {
    pub fn new() -> Self {
        Self {
            table: TyTable::new(),
            map: HashMap::new(),
        }
    }

    pub fn lower_ty(&mut self, ty: &Type) -> TyId {
        if let Some(&id) = self.map.get(ty) {
            return id;
        }

        let id = match ty {
            // Scalar Types
            Type::Unit => self.table.add(TyKind::Unit),
            Type::Bool => self.table.add(TyKind::Bool),
            Type::Char => self.table.add(TyKind::Int {
                bits: 32,
                signed: false,
            }),
            Type::UInt64 => self.table.add(TyKind::Int {
                bits: 64,
                signed: false,
            }),
            Type::UInt32 => self.table.add(TyKind::Int {
                bits: 32,
                signed: false,
            }),
            Type::UInt8 => self.table.add(TyKind::Int {
                bits: 8,
                signed: false,
            }),
            Type::Range { .. } => self.lower_ty(&Type::UInt64),

            // Aggregate Types
            Type::Array { elem_ty, dims } => {
                let elem_id = self.lower_ty(elem_ty);
                self.table.add(TyKind::Array {
                    elem_ty: elem_id,
                    dims: dims.clone(),
                })
            }
            Type::Tuple { fields } => {
                let ids = fields.iter().map(|t| self.lower_ty(t)).collect();
                self.table.add(TyKind::Tuple { field_tys: ids })
            }
            Type::Struct { name, fields } => {
                let name = name.clone();
                let struct_fields = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.clone(),
                        ty: self.lower_ty(&f.ty),
                    })
                    .collect();
                self.table.add_named(
                    TyKind::Struct {
                        fields: struct_fields,
                    },
                    name,
                )
            }
            Type::Enum { name, variants } => {
                let name = name.clone();
                // Enums are modeled as a tuple of (scalar tag, blob of bytes for the payload).
                // When lowering a variant, we compute the offset of each payload element in
                // the blob and use that to project the payload elements.
                let max_payload_size = variants
                    .iter()
                    .map(|v| v.payload.iter().map(|p| p.size_of()).sum::<usize>())
                    .max()
                    .unwrap_or(0);

                // tag type (u64)
                let tag_kind = TyKind::Int {
                    signed: false,
                    bits: 64,
                };

                if max_payload_size == 0 {
                    // no payload, just a scalar tag
                    self.table.add_named(tag_kind, name)
                } else {
                    let tag_ty_id = self.table.add(tag_kind);
                    // blob type (u8 array)
                    let u8_ty_id = self.table.add(TyKind::Int {
                        signed: false,
                        bits: 8,
                    });
                    let blob_ty_id = self.table.add(TyKind::Array {
                        elem_ty: u8_ty_id,
                        dims: vec![max_payload_size],
                    });

                    // tuple type (tag, blob)
                    self.table.add_named(
                        TyKind::Tuple {
                            field_tys: vec![tag_ty_id, blob_ty_id],
                        },
                        name,
                    )
                }
            }
            Type::String => {
                // Map to a struct { ptr, len, tag }
                let u64_id = self.lower_ty(&Type::UInt64);
                let u32_id = self.lower_ty(&Type::UInt32);
                let u8_id = self.lower_ty(&Type::UInt8);

                self.table.add_named(
                    TyKind::Struct {
                        fields: vec![
                            StructField {
                                name: "ptr".to_string(),
                                ty: u64_id,
                            },
                            StructField {
                                name: "len".to_string(),
                                ty: u32_id,
                            },
                            StructField {
                                name: "tag".to_string(),
                                ty: u8_id,
                            },
                        ],
                    },
                    "string".to_string(),
                )
            }

            Type::Unknown => panic!("Cannot lower unknown type"),
        };

        self.map.insert(ty.clone(), id);
        id
    }
}
