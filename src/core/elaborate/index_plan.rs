//! Indexing and slicing plan construction.

use crate::core::tree::semantic as sem;
use crate::core::types::Type;

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    pub(super) fn build_index_plan(&self, target_ty: &Type) -> sem::IndexPlan {
        let (peeled_ty, deref_count) = target_ty.peel_heap_with_count();

        let base = match peeled_ty {
            Type::Array { dims, .. } => sem::IndexBaseKind::Array {
                dims: dims.iter().map(|dim| *dim as u64).collect(),
                deref_count,
            },
            Type::DynArray { .. } => sem::IndexBaseKind::DynArray { deref_count },
            Type::Slice { .. } => sem::IndexBaseKind::Slice { deref_count },
            Type::String => sem::IndexBaseKind::String { deref_count },
            _ => {
                panic!("compiler bug: invalid index target type (type checker should catch this)");
            }
        };

        sem::IndexPlan { base }
    }

    pub(super) fn build_slice_plan(&self, target_ty: &Type, slice_ty: &Type) -> sem::SlicePlan {
        let Type::Slice { elem_ty } = slice_ty else {
            panic!("compiler bug: non-slice target for slice expression");
        };

        let elem_size = elem_ty.size_of() as u64;
        let (peeled_ty, deref_count) = target_ty.peel_heap_with_count();

        let base = match peeled_ty {
            Type::Array { dims, .. } => {
                let len = dims
                    .first()
                    .copied()
                    .unwrap_or_else(|| panic!("compiler bug: empty array dims"));
                sem::SliceBaseKind::Array {
                    len: len as u64,
                    deref_count,
                }
            }
            Type::DynArray { .. } => sem::SliceBaseKind::DynArray { deref_count },
            Type::Slice { .. } => sem::SliceBaseKind::Slice { deref_count },
            Type::String => sem::SliceBaseKind::String { deref_count },
            _ => {
                panic!("compiler bug: invalid slice target type (type checker should catch this)");
            }
        };

        sem::SlicePlan { base, elem_size }
    }
}
