//! Executable entry wrapper generation for user `main`.
//!
//! This module injects a C-ABI-friendly entry shim that calls user `main`,
//! maps successful returns to process exit codes, and reports unhandled
//! error-union variants through the runtime trap path.

use crate::core::backend::lower::LoweredFunction;
use crate::core::backend::lower::globals::GlobalArena;
use crate::core::backend::lower::types::TypeLowerer;
use crate::core::ir::{
    Callee, CastKind, CmpOp, ConstValue, FunctionBuilder, FunctionSig, IrTypeId, RuntimeFn,
    SwitchCase, Terminator, ValueId,
};
use crate::core::resolve::{DefId, DefTable};
use crate::core::tree::semantic as sem;
use crate::core::typecheck::type_map::TypeMap;
use crate::core::types::Type;

const ENTRY_MAIN_WRAPPER_NAME: &str = "__mc_entry_main_wrapper";
const USER_MAIN_IMPL_NAME: &str = "__mc_user_main";

struct MainErrorCase {
    union_tag: u32,
    err_ty: Type,
    err_payload: Option<(IrTypeId, u64)>,
    fallback_label: String,
}

/// Generates the executable entry wrapper for the program `main` function.
///
/// High-level behavior:
/// - rename user `main` to `__mc_user_main`,
/// - generate C-ABI `main` shim (`__mc_entry_main_wrapper`),
/// - call user main and translate the result to process exit semantics,
/// - report unhandled error-union variants via `RuntimeFn::Trap`.
pub(super) fn append_executable_entry_wrapper(
    module: &sem::Module,
    def_table: &DefTable,
    type_map: &TypeMap,
    funcs: &mut Vec<LoweredFunction>,
    globals: &mut GlobalArena,
) {
    let Some(main_def) = module
        .func_defs()
        .into_iter()
        .find(|f| f.sig.name == "main")
    else {
        return;
    };

    let main_def_id = main_def.def_id;
    let Some(main_lowered) = funcs.iter_mut().find(|f| f.func.def_id == main_def_id) else {
        return;
    };

    let Some(main_def_row) = def_table.lookup_def(main_def_id) else {
        return;
    };
    let Some(main_fn_ty) = type_map.lookup_def_type(main_def_row) else {
        return;
    };
    let Type::Fn { params, ret_ty } = main_fn_ty else {
        return;
    };
    if !params.is_empty() {
        return;
    }

    // Keep user main callable under an internal symbol; wrapper becomes entrypoint.
    main_lowered.func.name = USER_MAIN_IMPL_NAME.to_string();

    let wrapper_def_id = DefId(
        funcs
            .iter()
            .map(|f| f.func.def_id.0)
            .max()
            .unwrap_or(0)
            .saturating_add(1),
    );

    let mut type_lowerer = TypeLowerer::new_with_type_defs(type_map, Some(def_table), Some(module));
    let u64_ty = type_lowerer.lower_type(&Type::uint(64));
    let unit_ty = type_lowerer.lower_type(&Type::Unit);
    let bool_ty = type_lowerer.lower_type(&Type::Bool);
    let sig = FunctionSig {
        params: Vec::new(),
        ret: u64_ty,
    };
    let mut builder = FunctionBuilder::new(wrapper_def_id, ENTRY_MAIN_WRAPPER_NAME, sig);

    let ret_ty = *ret_ty;
    let ret_ir_ty = type_lowerer.lower_type(&ret_ty);
    let ret_value = builder.call(Callee::Direct(main_def_id), Vec::new(), ret_ir_ty);

    match &ret_ty {
        Type::ErrorUnion { ok_ty, err_tys } => {
            // Materialize the returned union so we can inspect tag + payload fields.
            let slot_local = builder.add_local(ret_ir_ty, None);
            let slot_ptr_ty = type_lowerer.ptr_to(ret_ir_ty);
            let slot_addr = builder.addr_of_local(slot_local, slot_ptr_ty);
            builder.store(slot_addr, ret_value);

            let union_ty_id = type_map
                .type_table()
                .lookup_id(&ret_ty)
                .unwrap_or_else(|| panic!("backend wrapper missing type id for {:?}", ret_ty));
            let (tag_ty, blob_ty, ok_payload, err_cases) = {
                let layout = type_lowerer.enum_layout(union_ty_id);
                let ok_variant = layout.variants.first().unwrap_or_else(|| {
                    panic!("backend wrapper missing ok variant in {:?}", ret_ty)
                });
                let ok_payload = ok_variant
                    .field_tys
                    .first()
                    .copied()
                    .zip(ok_variant.field_offsets.first().copied());
                let err_cases = layout
                    .variants
                    .iter()
                    .skip(1)
                    .enumerate()
                    .map(|(idx, variant)| MainErrorCase {
                        union_tag: variant.tag,
                        err_ty: err_tys[idx].clone(),
                        err_payload: variant
                            .field_tys
                            .first()
                            .copied()
                            .zip(variant.field_offsets.first().copied()),
                        fallback_label: format_error_label(&err_tys[idx]),
                    })
                    .collect::<Vec<_>>();
                (layout.tag_ty, layout.blob_ty, ok_payload, err_cases)
            };

            // Branch on union tag: Ok path returns mapped exit code, Err path reports.
            let tag_ptr_ty = type_lowerer.ptr_to(tag_ty);
            let tag_ptr = builder.field_addr(slot_addr, 0, tag_ptr_ty);
            let tag = builder.load(tag_ptr, tag_ty);
            let ok_tag = builder.const_int(0, false, 32, tag_ty);
            let is_ok = builder.cmp(CmpOp::Eq, tag, ok_tag, bool_ty);

            let ok_bb = builder.add_block();
            let err_bb = builder.add_block();
            builder.terminate(Terminator::CondBr {
                cond: is_ok,
                then_bb: ok_bb,
                then_args: Vec::new(),
                else_bb: err_bb,
                else_args: Vec::new(),
            });

            builder.select_block(ok_bb);
            let ok_exit = map_ok_payload_to_exit_code(
                &mut builder,
                &mut type_lowerer,
                slot_addr,
                blob_ty,
                ok_ty,
                ok_payload,
                u64_ty,
            );
            builder.terminate(Terminator::Return {
                value: Some(ok_exit),
            });

            // On Err(tag), dispatch to one trap block per known error variant label.
            builder.select_block(err_bb);
            let default_bb = builder.add_block();
            let mut cases = Vec::with_capacity(err_cases.len());
            let mut case_blocks = Vec::with_capacity(err_cases.len());

            for err_case in err_cases {
                let bb = builder.add_block();
                cases.push(SwitchCase {
                    value: ConstValue::Int {
                        value: err_case.union_tag as i128,
                        signed: false,
                        bits: 32,
                    },
                    target: bb,
                    args: Vec::new(),
                });
                case_blocks.push((bb, err_case));
            }

            builder.terminate(Terminator::Switch {
                value: tag,
                cases,
                default: default_bb,
                default_args: Vec::new(),
            });

            for (bb, err_case) in case_blocks {
                builder.select_block(bb);
                emit_main_error_case_trap(
                    &mut builder,
                    &mut type_lowerer,
                    type_map,
                    globals,
                    slot_addr,
                    blob_ty,
                    err_case,
                    unit_ty,
                    u64_ty,
                );
            }

            builder.select_block(default_bb);
            emit_main_error_trap(
                &mut builder,
                &mut type_lowerer,
                globals,
                "Unhandled error in main".to_string(),
                unit_ty,
                u64_ty,
            );
        }
        _ => {
            // Non-union return path: map direct return value to process exit code.
            let exit =
                map_direct_main_return_to_exit_code(&mut builder, ret_value, &ret_ty, u64_ty);
            builder.terminate(Terminator::Return { value: Some(exit) });
        }
    }

    funcs.push(LoweredFunction {
        func: builder.finish(),
        types: type_lowerer.ir_type_cache,
        globals: Vec::new(),
    });
}

/// Maps direct `main` return values to process exit code semantics.
///
/// Current policy:
/// - `()` => 0
/// - `bool` => 0/1
/// - integer => widened/truncated to u64 as needed
/// - non-scalar => 0 (MVP fallback)
fn map_direct_main_return_to_exit_code(
    builder: &mut FunctionBuilder,
    value: ValueId,
    ret_ty: &Type,
    u64_ty: IrTypeId,
) -> ValueId {
    match ret_ty {
        Type::Unit => builder.const_int(0, false, 64, u64_ty),

        Type::Bool => builder.int_extend(value, u64_ty, false),

        Type::Int { signed, bits, .. } => {
            if *bits < 64 {
                builder.int_extend(value, u64_ty, *signed)
            } else if *bits > 64 {
                builder.int_trunc(value, u64_ty)
            } else {
                value
            }
        }

        // Non-scalar success payloads are not mappable to process exit code in v1.
        _ => builder.const_int(0, false, 64, u64_ty),
    }
}

/// Maps the Ok payload of an error-union return to a process exit code.
///
/// Extracts the payload from the union slot and delegates to
/// [`map_direct_main_return_to_exit_code`] for the actual mapping.
fn map_ok_payload_to_exit_code(
    builder: &mut FunctionBuilder,
    type_lowerer: &mut TypeLowerer<'_>,
    union_slot_addr: ValueId,
    blob_ty: IrTypeId,
    ok_sem_ty: &Type,
    ok_payload: Option<(IrTypeId, u64)>,
    u64_ty: IrTypeId,
) -> ValueId {
    let Some((payload_ty, payload_offset)) = ok_payload else {
        return builder.const_int(0, false, 64, u64_ty);
    };

    let payload_ptr = union_payload_addr(
        builder,
        type_lowerer,
        union_slot_addr,
        blob_ty,
        payload_ty,
        payload_offset,
        u64_ty,
    );
    let payload_val = builder.load(payload_ptr, payload_ty);

    map_direct_main_return_to_exit_code(builder, payload_val, ok_sem_ty, u64_ty)
}

/// Emits a trap call for unhandled `main` errors with a static message payload.
///
/// Uses trap kind `6`, interpreted by runtime as:
/// - arg0: pointer to UTF-8 bytes
/// - arg1: message length
/// - arg2: unused
fn emit_main_error_trap(
    builder: &mut FunctionBuilder,
    type_lowerer: &mut TypeLowerer<'_>,
    globals: &mut GlobalArena,
    message: String,
    unit_ty: IrTypeId,
    u64_ty: IrTypeId,
) {
    let bytes = message.into_bytes();
    let msg_len = bytes.len() as i128;
    let global_id = globals.add_bytes(bytes);

    let u8_ty = type_lowerer.lower_type(&Type::uint(8));
    let u8_ptr_ty = type_lowerer.ptr_to(u8_ty);
    let msg_ptr = builder.const_global_addr(global_id, u8_ptr_ty);

    let msg_ptr_u64 = builder.cast(CastKind::PtrToInt, msg_ptr, u64_ty);
    let msg_len_u64 = builder.const_int(msg_len, false, 64, u64_ty);

    let kind = builder.const_int(6, false, 64, u64_ty);
    let zero = builder.const_int(0, false, 64, u64_ty);

    let _ = builder.call(
        Callee::Runtime(RuntimeFn::Trap),
        vec![kind, msg_ptr_u64, msg_len_u64, zero],
        unit_ty,
    );

    builder.terminate(Terminator::Unreachable);
}

#[allow(clippy::too_many_arguments)]
fn emit_main_error_case_trap(
    builder: &mut FunctionBuilder,
    type_lowerer: &mut TypeLowerer<'_>,
    type_map: &TypeMap,
    globals: &mut GlobalArena,
    union_slot_addr: ValueId,
    union_blob_ty: IrTypeId,
    err_case: MainErrorCase,
    unit_ty: IrTypeId,
    u64_ty: IrTypeId,
) {
    if let Type::Enum { name, .. } = &err_case.err_ty
        && let Some((payload_ty, payload_offset)) = err_case.err_payload
        && let Some(enum_ty_id) = type_map.type_table().lookup_id(&err_case.err_ty)
    {
        let enum_addr = union_payload_addr(
            builder,
            type_lowerer,
            union_slot_addr,
            union_blob_ty,
            payload_ty,
            payload_offset,
            u64_ty,
        );
        emit_enum_variant_main_error_trap(
            builder,
            type_lowerer,
            globals,
            enum_addr,
            enum_ty_id,
            name,
            unit_ty,
            u64_ty,
        );
        return;
    }

    emit_main_error_trap(
        builder,
        type_lowerer,
        globals,
        format!("Unhandled error in main: {}", err_case.fallback_label),
        unit_ty,
        u64_ty,
    );
}

#[allow(clippy::too_many_arguments)]
fn emit_enum_variant_main_error_trap(
    builder: &mut FunctionBuilder,
    type_lowerer: &mut TypeLowerer<'_>,
    globals: &mut GlobalArena,
    enum_addr: ValueId,
    enum_ty_id: crate::core::types::TypeId,
    enum_name: &str,
    unit_ty: IrTypeId,
    u64_ty: IrTypeId,
) {
    let (tag_ty, variants) = {
        let layout = type_lowerer.enum_layout(enum_ty_id);
        (layout.tag_ty, layout.variants.clone())
    };

    let enum_tag_ptr_ty = type_lowerer.ptr_to(tag_ty);
    let enum_tag_ptr = builder.field_addr(enum_addr, 0, enum_tag_ptr_ty);
    let enum_tag = builder.load(enum_tag_ptr, tag_ty);

    let default_bb = builder.add_block();
    let mut cases = Vec::with_capacity(variants.len());
    let mut case_blocks = Vec::with_capacity(variants.len());
    for variant in variants {
        let bb = builder.add_block();
        cases.push(SwitchCase {
            value: ConstValue::Int {
                value: variant.tag as i128,
                signed: false,
                bits: 32,
            },
            target: bb,
            args: Vec::new(),
        });
        case_blocks.push((bb, variant.name));
    }

    builder.terminate(Terminator::Switch {
        value: enum_tag,
        cases,
        default: default_bb,
        default_args: Vec::new(),
    });

    for (bb, variant_name) in case_blocks {
        builder.select_block(bb);
        emit_main_error_trap(
            builder,
            type_lowerer,
            globals,
            format!("Unhandled error in main: {enum_name}::{variant_name}"),
            unit_ty,
            u64_ty,
        );
    }

    builder.select_block(default_bb);
    emit_main_error_trap(
        builder,
        type_lowerer,
        globals,
        format!("Unhandled error in main: {enum_name}"),
        unit_ty,
        u64_ty,
    );
}

fn union_payload_addr(
    builder: &mut FunctionBuilder,
    type_lowerer: &mut TypeLowerer<'_>,
    union_slot_addr: ValueId,
    blob_ty: IrTypeId,
    payload_ty: IrTypeId,
    payload_offset: u64,
    u64_ty: IrTypeId,
) -> ValueId {
    let blob_ptr_ty = type_lowerer.ptr_to(blob_ty);
    let blob_ptr = builder.field_addr(union_slot_addr, 1, blob_ptr_ty);

    let u8_ty = type_lowerer.lower_type(&Type::uint(8));
    let u8_ptr_ty = type_lowerer.ptr_to(u8_ty);
    let payload_u8_ptr = builder.cast(CastKind::PtrToPtr, blob_ptr, u8_ptr_ty);

    let payload_off = builder.const_int(payload_offset as i128, false, 64, u64_ty);
    let payload_addr_u8 = builder.index_addr(payload_u8_ptr, payload_off, u8_ptr_ty);

    let payload_ptr_ty = type_lowerer.ptr_to(payload_ty);
    builder.cast(CastKind::PtrToPtr, payload_addr_u8, payload_ptr_ty)
}

/// Produces the user-facing label for an error-union variant.
///
/// We prefer nominal type names where available because there is no display/format
/// trait system in runtime yet.
fn format_error_label(ty: &Type) -> String {
    match ty {
        Type::Struct { name, .. } | Type::Enum { name, .. } => name.clone(),
        _ => ty.to_string(),
    }
}
