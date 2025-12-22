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
            Type::UInt64 => self.table.add(TyKind::Int {
                bits: 64,
                signed: false,
            }),

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
            Type::Struct { fields, .. } => {
                let struct_fields = fields
                    .iter()
                    .map(|f| StructField {
                        name: f.name.clone(),
                        ty: self.lower_ty(&f.ty),
                    })
                    .collect();
                self.table.add(TyKind::Struct {
                    fields: struct_fields,
                })
            }
            Type::Enum { .. } => {
                // Treat as a scalar tag for now
                self.table.add(TyKind::Int {
                    signed: false,
                    bits: 64,
                })
            }

            Type::Unknown => panic!("Cannot lower unknown type"),
        };

        self.map.insert(ty.clone(), id);
        id
    }
}
