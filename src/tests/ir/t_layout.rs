use crate::core::ir::{IrStructField, IrTypeCache, IrTypeKind};

#[test]
fn test_layout_tuple_offsets() {
    let mut types = IrTypeCache::new();
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let tuple_ty = types.add(IrTypeKind::Tuple {
        fields: vec![u8_ty, u64_ty],
    });

    let layout = types.layout(tuple_ty);

    assert_eq!(layout.size(), 16);
    assert_eq!(layout.align(), 8);
    assert_eq!(layout.field_offsets(), &[0u64, 8]);
    assert_eq!(layout.stride(), 16);
}

#[test]
fn test_layout_struct_offsets() {
    let mut types = IrTypeCache::new();
    let u8_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 8,
    });
    let u16_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 16,
    });
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let struct_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: u8_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "c".to_string(),
                ty: u16_ty,
            },
        ],
    });

    let layout = types.layout(struct_ty);

    assert_eq!(layout.size(), 24);
    assert_eq!(layout.align(), 8);
    assert_eq!(layout.field_offsets(), &[0u64, 8, 16]);
    assert_eq!(layout.stride(), 24);
}

#[test]
fn test_layout_array_stride() {
    let mut types = IrTypeCache::new();
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let array_ty = types.add(IrTypeKind::Array {
        elem: u64_ty,
        dims: vec![3],
    });

    let layout = types.layout(array_ty);

    assert_eq!(layout.size(), 24);
    assert_eq!(layout.align(), 8);
    assert_eq!(layout.field_offsets(), &[]);
    assert_eq!(layout.stride(), 8);
}

#[test]
fn test_layout_blob() {
    let mut types = IrTypeCache::new();
    let blob_ty = types.add(IrTypeKind::Blob { size: 24, align: 8 });

    let layout = types.layout(blob_ty);

    assert_eq!(layout.size(), 24);
    assert_eq!(layout.align(), 8);
    assert_eq!(layout.field_offsets(), &[]);
    assert_eq!(layout.stride(), 24);
}

#[test]
fn test_scalar_size_and_sret_helpers() {
    let mut types = IrTypeCache::new();
    let bool_ty = types.add(IrTypeKind::Bool);
    let u64_ty = types.add(IrTypeKind::Int {
        signed: false,
        bits: 64,
    });
    let pair_ty = types.add(IrTypeKind::Struct {
        fields: vec![
            IrStructField {
                name: "a".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "b".to_string(),
                ty: u64_ty,
            },
            IrStructField {
                name: "c".to_string(),
                ty: u64_ty,
            },
        ],
    });

    let bool_layout = types.layout(bool_ty);
    assert_eq!(types.scalar_size_for_layout(bool_ty, &bool_layout), 1);
    assert!(!types.needs_sret_for_layout(bool_ty, &bool_layout));

    let pair_layout = types.layout(pair_ty);
    assert_eq!(types.scalar_size_for_layout(pair_ty, &pair_layout), 24);
    assert!(types.needs_sret_for_layout(pair_ty, &pair_layout));
}
