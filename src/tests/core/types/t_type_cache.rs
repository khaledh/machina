use super::*;
use crate::core::types::{EnumVariant, StructField};

#[test]
fn test_intern_upgrades_struct_when_richer_type_arrives() {
    let mut cache = TypeCache::new();
    let node_shallow = Type::Struct {
        name: "Node".to_string(),
        fields: Vec::new(),
    };
    let id = cache.intern(node_shallow);
    let node_full = Type::Struct {
        name: "Node".to_string(),
        fields: vec![StructField {
            name: "value".to_string(),
            ty: Type::uint(64),
        }],
    };

    let id2 = cache.intern(node_full);
    assert_eq!(id.index(), id2.index());
    match cache.get(id) {
        Type::Struct { fields, .. } => assert_eq!(fields.len(), 1),
        other => panic!("expected struct type, got {other:?}"),
    }
}

#[test]
fn test_intern_keeps_richer_enum_when_shallow_seen_later() {
    let mut cache = TypeCache::new();
    let link_full = Type::Enum {
        name: "Link".to_string(),
        variants: vec![
            EnumVariant {
                name: "None".to_string(),
                payload: Vec::new(),
            },
            EnumVariant {
                name: "Some".to_string(),
                payload: vec![Type::Heap {
                    elem_ty: Box::new(Type::Struct {
                        name: "Node".to_string(),
                        fields: vec![StructField {
                            name: "next".to_string(),
                            ty: Type::Enum {
                                name: "Link".to_string(),
                                variants: Vec::new(),
                            },
                        }],
                    }),
                }],
            },
        ],
    };
    let id = cache.intern(link_full);
    let link_shallow = Type::Enum {
        name: "Link".to_string(),
        variants: Vec::new(),
    };
    let id2 = cache.intern(link_shallow);

    assert_eq!(id.index(), id2.index());
    match cache.get(id) {
        Type::Enum { variants, .. } => assert_eq!(variants.len(), 2),
        other => panic!("expected enum type, got {other:?}"),
    }
}
