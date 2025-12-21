use crate::ast::{Expr, ExprKind};
use crate::ir::types::{IrAddr, IrType};
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ByteOffset(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LinearIndex(pub usize);

pub fn lower_type(ty: &Type) -> IrType {
    match ty {
        Type::UInt64 => IrType::Int {
            bits: 64,
            signed: false,
        },
        Type::Bool => IrType::Bool,
        Type::Unit => IrType::Int {
            bits: 1,
            signed: false,
        },
        Type::Unknown => panic!("Unknown type"),
        Type::Array { elem_ty, dims } => IrType::Array {
            elem_ty: Box::new(lower_type(elem_ty)),
            dims: dims.clone(),
        },
        Type::Tuple { fields } => IrType::Tuple {
            fields: fields.iter().map(lower_type).collect(),
        },
        Type::Struct { fields, .. } => IrType::Tuple {
            // structs are lowered as tuples of their fields
            fields: fields.iter().map(|f| lower_type(&f.ty)).collect(),
        },
    }
}

pub fn tuple_field_byte_offset(ty: &IrType, field_index: usize) -> ByteOffset {
    ByteOffset(ty.tuple_field_offset(field_index))
}

pub fn struct_field_byte_offset(ty: &IrType, field_name: &str) -> ByteOffset {
    ByteOffset(ty.struct_field_offset(field_name))
}

pub fn try_const_fold_array_linear_index(dims: &[usize], indices: &[Expr]) -> Option<LinearIndex> {
    let const_indices: Vec<usize> = indices
        .iter()
        .map(|idx| match idx.kind {
            ExprKind::UInt64Lit(val) => Some(val as usize),
            _ => None,
        })
        .collect::<Option<Vec<_>>>()?;

    // Compute linear index: i0 * (d1 * d2 * ... * dn) + i1 * (d2 * d3 * ... * dn) + ... + in
    let mut linear_index = 0usize;
    for (i, &idx) in const_indices.iter().enumerate() {
        let stride: usize = dims[i + 1..].iter().product();
        linear_index += idx * stride;
    }
    Some(LinearIndex(linear_index))
}

impl IrAddr {
    pub fn tuple_field(addr: &IrAddr, field_index: usize) -> IrAddr {
        debug_assert!(
            matches!(addr.ty, IrType::Tuple { .. }),
            "Expected tuple type"
        );
        IrAddr {
            base: addr.base,
            offset: addr.offset + addr.ty.tuple_field_offset(field_index),
            ty: addr.ty.tuple_field_type(field_index),
        }
    }

    pub fn struct_field(addr: &IrAddr, field_name: &str) -> IrAddr {
        debug_assert!(
            matches!(addr.ty, IrType::Struct { .. }),
            "Expected struct type"
        );
        IrAddr {
            base: addr.base,
            offset: addr.offset + addr.ty.struct_field_offset(field_name),
            ty: addr.ty.struct_field_type(field_name),
        }
    }
}
