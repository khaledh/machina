use std::collections::HashMap;

use crate::ssa::{IrTypeId, IrTypeInfo, IrTypeKind};

/// Layout information for an SSA IR type.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IrLayout {
    size: u64,
    align: u64,
    field_offsets: Vec<u64>,
    stride: u64,
}

impl IrLayout {
    pub fn size(&self) -> u64 {
        self.size
    }

    pub fn align(&self) -> u64 {
        self.align
    }

    pub fn field_offsets(&self) -> &[u64] {
        &self.field_offsets
    }

    pub fn stride(&self) -> u64 {
        self.stride
    }
}

fn align_to(value: u64, align: u64) -> u64 {
    debug_assert!(align != 0);
    (value + align - 1) & !(align - 1)
}

/// Caches computed layouts for SSA types.
#[derive(Debug, Default, Clone, PartialEq, Eq)]
pub struct IrLayoutCache {
    layouts: HashMap<IrTypeId, IrLayout>,
}

impl IrLayoutCache {
    pub fn new() -> Self {
        Self {
            layouts: HashMap::new(),
        }
    }

    /// Returns the cached layout or computes it on demand.
    pub fn layout(&mut self, types: &[IrTypeInfo], ty: IrTypeId) -> IrLayout {
        if let Some(layout) = self.layouts.get(&ty) {
            return layout.clone();
        }

        let layout = self.compute_layout(types, ty);
        self.layouts.insert(ty, layout.clone());
        layout
    }

    pub fn invalidate(&mut self, ty: IrTypeId) {
        self.layouts.remove(&ty);
    }

    fn compute_layout(&mut self, types: &[IrTypeInfo], ty: IrTypeId) -> IrLayout {
        match &types[ty.index()].kind {
            IrTypeKind::Unit => IrLayout {
                size: 0,
                align: 1,
                field_offsets: Vec::new(),
                stride: 0,
            },
            IrTypeKind::Bool => IrLayout {
                size: 1,
                align: 1,
                field_offsets: Vec::new(),
                stride: 1,
            },
            IrTypeKind::Int { bits, .. } => {
                let size = (*bits as u64) / 8;
                IrLayout {
                    size,
                    align: size.max(1),
                    field_offsets: Vec::new(),
                    stride: size,
                }
            }
            IrTypeKind::Ptr { .. } | IrTypeKind::Fn { .. } => IrLayout {
                size: 8,
                align: 8,
                field_offsets: Vec::new(),
                stride: 8,
            },
            IrTypeKind::Array { elem, dims } => {
                let elem_layout = self.layout(types, *elem);
                let stride = align_to(elem_layout.size, elem_layout.align);
                let count = dims.iter().product::<u64>();
                IrLayout {
                    size: stride.saturating_mul(count),
                    align: elem_layout.align,
                    field_offsets: Vec::new(),
                    stride,
                }
            }
            IrTypeKind::Tuple { fields } => self.layout_fields(types, fields.iter().copied()),
            IrTypeKind::Struct { fields } => {
                self.layout_fields(types, fields.iter().map(|field| field.ty))
            }
            IrTypeKind::Blob { size, align } => IrLayout {
                size: *size,
                align: *align,
                field_offsets: Vec::new(),
                stride: *size,
            },
        }
    }

    fn layout_fields(
        &mut self,
        types: &[IrTypeInfo],
        fields: impl Iterator<Item = IrTypeId>,
    ) -> IrLayout {
        let mut offsets = Vec::new();
        let mut offset = 0u64;
        let mut max_align = 1u64;

        for field_ty in fields {
            let field_layout = self.layout(types, field_ty);
            offset = align_to(offset, field_layout.align);
            offsets.push(offset);
            offset += field_layout.size;
            max_align = max_align.max(field_layout.align);
        }

        let size = align_to(offset, max_align);
        IrLayout {
            size,
            align: max_align,
            field_offsets: offsets,
            stride: size,
        }
    }
}

#[cfg(test)]
#[path = "../../tests/ssa/model/t_layout.rs"]
mod tests;
