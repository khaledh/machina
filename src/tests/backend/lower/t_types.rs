use crate::backend::lower::types::TypeLowerer;
use crate::context::{ParsedContext, SemanticContext};
use crate::elaborate::elaborate;
use crate::ir::IrTypeKind;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::tree::semantic as sem;
use crate::typecheck::type_check;
use crate::typecheck::type_map::resolve_type_expr;
use crate::types::{EnumVariant, FnParam, FnParamMode, Type, TypeId};
use indoc::indoc;

fn analyze(source: &str) -> SemanticContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

fn enum_type_id(ctx: &SemanticContext, name: &str) -> TypeId {
    let type_def = ctx
        .module
        .type_defs()
        .into_iter()
        .find(|def| def.name == name)
        .unwrap_or_else(|| panic!("missing enum type def {name}"));
    let variants = match &type_def.kind {
        sem::TypeDefKind::Enum { variants } => variants,
        other => panic!("expected enum type def, found {:?}", other),
    };

    let mut enum_variants = Vec::new();
    for variant in variants {
        let payload = variant
            .payload
            .iter()
            .map(|ty_expr| resolve_type_expr(&ctx.def_table, &ctx.module, ty_expr))
            .collect::<Result<Vec<Type>, _>>()
            .unwrap_or_else(|errs| panic!("failed to resolve enum payload types: {:?}", errs));
        enum_variants.push(EnumVariant {
            name: variant.name.clone(),
            payload,
        });
    }

    let enum_ty = Type::Enum {
        name: type_def.name.clone(),
        variants: enum_variants,
    };
    ctx.type_map
        .type_table()
        .lookup_id(&enum_ty)
        .unwrap_or_else(|| panic!("missing type id for enum {name}"))
}

#[test]
fn test_lower_slice_type() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            0
        }
    "});

    let mut type_lowerer = TypeLowerer::new(&ctx.type_map);
    let slice_ty = Type::Slice {
        elem_ty: Box::new(Type::uint(8)),
    };
    let slice_ir = type_lowerer.lower_type(&slice_ty);

    let IrTypeKind::Struct { fields } = type_lowerer.ir_type_cache.kind(slice_ir) else {
        panic!("expected slice to lower to a struct");
    };
    assert_eq!(fields.len(), 2);
    assert_eq!(fields[0].name, "ptr");
    assert_eq!(fields[1].name, "len");

    let IrTypeKind::Ptr { elem } = type_lowerer.ir_type_cache.kind(fields[0].ty) else {
        panic!("expected slice ptr field to be a pointer");
    };
    match type_lowerer.ir_type_cache.kind(*elem) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 8);
        }
        other => panic!("expected u8 slice element, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(fields[1].ty) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 64);
        }
        other => panic!("expected u64 slice len, got {:?}", other),
    }
}

#[test]
fn test_lower_fn_type() {
    let ctx = analyze(indoc! {"
        fn main() -> u64 {
            0
        }
    "});

    let mut type_lowerer = TypeLowerer::new(&ctx.type_map);
    let fn_ty = Type::Fn {
        params: vec![
            FnParam {
                mode: FnParamMode::In,
                ty: Type::uint(64),
            },
            FnParam {
                mode: FnParamMode::Out,
                ty: Type::uint(8),
            },
            FnParam {
                mode: FnParamMode::InOut,
                ty: Type::uint(16),
            },
            FnParam {
                mode: FnParamMode::Sink,
                ty: Type::uint(32),
            },
        ],
        ret_ty: Box::new(Type::uint(64)),
    };
    let ir_fn = type_lowerer.lower_type(&fn_ty);

    let IrTypeKind::Fn { params, ret } = type_lowerer.ir_type_cache.kind(ir_fn) else {
        panic!("expected fn type to lower to IrTypeKind::Fn");
    };
    assert_eq!(params.len(), 4);

    match type_lowerer.ir_type_cache.kind(params[0]) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 64);
        }
        other => panic!("expected in param to be u64, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(params[1]) {
        IrTypeKind::Ptr { elem } => match type_lowerer.ir_type_cache.kind(*elem) {
            IrTypeKind::Int { signed, bits } => {
                assert!(!signed);
                assert_eq!(*bits, 8);
            }
            other => panic!("expected out param elem to be u8, got {:?}", other),
        },
        other => panic!("expected out param to be ptr, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(params[2]) {
        IrTypeKind::Ptr { elem } => match type_lowerer.ir_type_cache.kind(*elem) {
            IrTypeKind::Int { signed, bits } => {
                assert!(!signed);
                assert_eq!(*bits, 16);
            }
            other => panic!("expected inout param elem to be u16, got {:?}", other),
        },
        other => panic!("expected inout param to be ptr, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(params[3]) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 32);
        }
        other => panic!("expected sink param to be u32, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(*ret) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 64);
        }
        other => panic!("expected fn ret to be u64, got {:?}", other),
    }
}

#[test]
fn test_enum_layout_single_payload() {
    let ctx = analyze(indoc! {"
        type Option = None | Some(u64)

        fn main() -> Option {
            Option::Some(1)
        }
    "});
    let enum_ty_id = enum_type_id(&ctx, "Option");

    let mut type_lowerer = TypeLowerer::new(&ctx.type_map);
    let (tag_ty, blob_ty, v0, v1) = {
        let layout = type_lowerer.enum_layout(enum_ty_id);
        (
            layout.tag_ty,
            layout.blob_ty,
            (
                layout.variants[0].tag,
                layout.variants[0].payload_size,
                layout.variants[0].payload_align,
                layout.variants[0].field_offsets.clone(),
            ),
            (
                layout.variants[1].tag,
                layout.variants[1].payload_size,
                layout.variants[1].payload_align,
                layout.variants[1].field_offsets.clone(),
            ),
        )
    };

    assert_eq!(v0.0, 0);
    assert_eq!(v0.1, 0);
    assert_eq!(v0.2, 1);
    assert_eq!(v0.3, Vec::<u64>::new());

    assert_eq!(v1.0, 1);
    assert_eq!(v1.1, 8);
    assert_eq!(v1.2, 8);
    assert_eq!(v1.3, vec![0u64]);

    match type_lowerer.ir_type_cache.kind(tag_ty) {
        IrTypeKind::Int { signed, bits } => {
            assert!(!signed);
            assert_eq!(*bits, 32);
        }
        other => panic!("expected u32 tag type, got {:?}", other),
    }

    match type_lowerer.ir_type_cache.kind(blob_ty) {
        IrTypeKind::Blob { size, align } => {
            assert_eq!(*size, 8);
            assert_eq!(*align, 8);
        }
        other => panic!("expected blob payload type, got {:?}", other),
    }
}

#[test]
fn test_enum_layout_multi_payload() {
    let ctx = analyze(indoc! {"
        type Pair = A(u8, u64) | B(u16)

        fn main() -> Pair {
            let a: u8 = 1;
            let b: u64 = 2;
            Pair::A(a, b)
        }
    "});
    let enum_ty_id = enum_type_id(&ctx, "Pair");

    let mut type_lowerer = TypeLowerer::new(&ctx.type_map);
    let (blob_ty, v0, v1) = {
        let layout = type_lowerer.enum_layout(enum_ty_id);
        (
            layout.blob_ty,
            (
                layout.variants[0].tag,
                layout.variants[0].payload_size,
                layout.variants[0].payload_align,
                layout.variants[0].field_offsets.clone(),
            ),
            (
                layout.variants[1].tag,
                layout.variants[1].payload_size,
                layout.variants[1].payload_align,
                layout.variants[1].field_offsets.clone(),
            ),
        )
    };

    assert_eq!(v0.0, 0);
    assert_eq!(v0.1, 16);
    assert_eq!(v0.2, 8);
    assert_eq!(v0.3, vec![0u64, 8]);

    assert_eq!(v1.0, 1);
    assert_eq!(v1.1, 2);
    assert_eq!(v1.2, 2);
    assert_eq!(v1.3, vec![0u64]);

    match type_lowerer.ir_type_cache.kind(blob_ty) {
        IrTypeKind::Blob { size, align } => {
            assert_eq!(*size, 16);
            assert_eq!(*align, 8);
        }
        other => panic!("expected blob payload type, got {:?}", other),
    }
}

#[test]
fn test_enum_ir_type_is_tagged_struct() {
    let ctx = analyze(indoc! {"
        type Flag = Off | On

        fn main() -> Flag {
            Flag::On
        }
    "});
    let enum_ty_id = enum_type_id(&ctx, "Flag");

    let mut type_lowerer = TypeLowerer::new(&ctx.type_map);
    let enum_ty = ctx.type_map.type_table().get(enum_ty_id).clone();
    let ir_ty = type_lowerer.lower_type(&enum_ty);

    match type_lowerer.ir_type_cache.kind(ir_ty) {
        IrTypeKind::Struct { fields } => {
            assert_eq!(fields.len(), 2);
            assert_eq!(fields[0].name, "tag");
            assert_eq!(fields[1].name, "payload");
        }
        other => panic!("expected enum to lower to struct, got {:?}", other),
    }
}
