use crate::mcir::types::{TyId, TyKind, TyTable};

/// Conservative size in bytes for scalar and aggregate types.
pub fn size_of_ty(types: &TyTable, ty: TyId) -> usize {
    match types.kind(ty) {
        TyKind::Unit => 0,
        TyKind::Bool => 1,
        TyKind::Int { bits, .. } => (*bits as usize).div_ceil(8),
        TyKind::Array { elem_ty, dims } => {
            let elems: usize = dims.iter().product();
            elems * size_of_ty(types, *elem_ty)
        }
        TyKind::Tuple { field_tys } => field_tys.iter().map(|ty| size_of_ty(types, *ty)).sum(),
        TyKind::Struct { fields } => fields.iter().map(|field| size_of_ty(types, field.ty)).sum(),
    }
}
