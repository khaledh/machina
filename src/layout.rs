use crate::ast::{Expr, ExprKind};
use crate::types::Type;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ByteOffset(pub usize);

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct LinearIndex(pub usize);

pub fn tuple_field_byte_offset(ty: &Type, field_index: usize) -> ByteOffset {
    ByteOffset(ty.tuple_field_offset(field_index))
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
