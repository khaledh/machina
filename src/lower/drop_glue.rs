use std::collections::HashMap;

use crate::lower::lower_ty::TyLowerer;
use crate::mcir::FuncBody;
use crate::mcir::abi::RuntimeFn;
use crate::mcir::func_builder::FuncBuilder;
use crate::mcir::types::{
    BlockId, Callee, Const, LocalId, LocalKind, Operand, PlaceAny, SwitchCase, Terminator,
};
use crate::mcir::types::{Place, Projection, Statement};
use crate::resolve::def_map::DefId;
use crate::types::Type;

#[derive(Debug)]
pub struct GeneratedDropGlue {
    pub def_id: DefId,
    pub name: String,
    pub body: FuncBody,
}

#[derive(Debug)]
enum GlueState {
    Generating,
    Ready,
}

#[derive(Debug)]
struct GlueEntry {
    def_id: DefId,
    state: GlueState,
}

#[derive(Debug)]
pub struct DropGlueRegistry {
    next_def_id: u32,
    by_type: HashMap<Type, GlueEntry>,
    generated: Vec<GeneratedDropGlue>,
}

impl DropGlueRegistry {
    pub fn new(next_def_id: DefId) -> Self {
        Self {
            next_def_id: next_def_id.0,
            by_type: HashMap::new(),
            generated: Vec::new(),
        }
    }

    pub fn get_or_create(&mut self, ty: &Type) -> DefId {
        if let Some(entry) = self.by_type.get(ty) {
            return entry.def_id;
        }

        let def_id = DefId(self.next_def_id);
        self.next_def_id += 1;

        self.by_type.insert(
            ty.clone(),
            GlueEntry {
                def_id,
                state: GlueState::Generating,
            },
        );

        let name = format!("__mc_drop${}", def_id.0);
        let body = build_drop_glue_body(ty, self);

        if let Some(entry) = self.by_type.get_mut(ty) {
            entry.state = GlueState::Ready
        }

        self.generated
            .push(GeneratedDropGlue { def_id, name, body });

        def_id
    }

    pub fn drain(self) -> Vec<GeneratedDropGlue> {
        self.generated
    }
}

fn build_drop_glue_body(ty: &Type, registry: &mut DropGlueRegistry) -> FuncBody {
    let mut ty_lowerer = TyLowerer::new();
    let ret_ty = ty_lowerer.lower_ty(&Type::Unit);
    let param_ty = ty_lowerer.lower_ty(ty);

    let mut fb = FuncBuilder::new(ret_ty);
    let entry = fb.body.entry;
    let param_local = fb.new_local(
        param_ty,
        LocalKind::Param { index: 0 },
        Some("value".to_string()),
    );

    emit_drop_for_type(&mut fb, &mut ty_lowerer, registry, entry, param_local, ty);

    // Some enum cases will override the terminator, so only set if still unterminated.
    if matches!(
        fb.body.blocks[entry.index()].terminator,
        Terminator::Unterminated
    ) {
        fb.set_terminator(entry, Terminator::Return);
    }

    fb.body.types = std::mem::take(&mut ty_lowerer.table);
    fb.body
}

// Rules
//
// * Type::Heap { elem_ty }
//   - If elem_ty.needs_drop(), call drop glue on *ptr (projection Deref).
//   - Always call runtime free on the pointer.
//
// * Struct / Tuple
//   - Drop fields in reverse order using Projection::Field.
//
// * Array
//   - Unroll indices in reverse order using Projection::Index with constant operands.
//   - For multi‑dim, element type becomes Type::Array { elem_ty, dims: dims[1..] }.
//
// * Enum
//   - If no payloads need drop, just Return.
//   - Otherwise:
//     * Load tag from Field { index: 0 }.
//     * Switch to a block per variant.
//     * In each variant block, drop payload fields via Field { index: 1 } + ByteOffset.
//     * Join to a return block.
//
fn emit_drop_for_type(
    fb: &mut FuncBuilder,
    ty_lowerer: &mut TyLowerer,
    registry: &mut DropGlueRegistry,
    block: BlockId,
    base: LocalId,
    ty: &Type,
) {
    if !ty.needs_drop() {
        return;
    }

    match ty {
        Type::Heap { elem_ty } => {
            let ptr_ty_id = ty_lowerer.lower_ty(ty);
            let ptr_place = Place::new(base, ptr_ty_id, vec![]);

            if elem_ty.needs_drop() {
                let elem_ty_id = ty_lowerer.lower_ty(elem_ty);
                let projs = vec![Projection::Deref];
                let pointee = if elem_ty.is_scalar() {
                    PlaceAny::Scalar(Place::new(base, elem_ty_id, projs))
                } else {
                    PlaceAny::Aggregate(Place::new(base, elem_ty_id, projs))
                };
                emit_drop_call(fb, registry, block, elem_ty, pointee);
            }

            // Free the heap pointer itself.
            fb.push_stmt(
                block,
                Statement::Call {
                    dst: None,
                    callee: Callee::Runtime(RuntimeFn::Free),
                    args: vec![PlaceAny::Scalar(ptr_place)],
                },
            );
        }

        Type::Struct { fields, .. } => {
            for (idx, field) in fields.iter().enumerate().rev() {
                if !field.ty.needs_drop() {
                    continue;
                }

                let proj = Projection::Field { index: idx };
                let field_ty_id = ty_lowerer.lower_ty(&field.ty);
                let field_place = if field.ty.is_scalar() {
                    PlaceAny::Scalar(Place::new(base, field_ty_id, vec![proj]))
                } else {
                    PlaceAny::Aggregate(Place::new(base, field_ty_id, vec![proj]))
                };
                emit_drop_call(fb, registry, block, &field.ty, field_place);
            }
        }

        Type::Tuple { fields } => {
            for (idx, elem_ty) in fields.iter().enumerate().rev() {
                if !elem_ty.needs_drop() {
                    continue;
                }

                let proj = Projection::Field { index: idx };
                let elem_ty_id = ty_lowerer.lower_ty(elem_ty);
                let elem_place = if elem_ty.is_scalar() {
                    PlaceAny::Scalar(Place::new(base, elem_ty_id, vec![proj]))
                } else {
                    PlaceAny::Aggregate(Place::new(base, elem_ty_id, vec![proj]))
                };
                emit_drop_call(fb, registry, block, elem_ty, elem_place);
            }
        }

        Type::Array { dims, .. } => {
            // Unroll fixed-size arrays; recurse on sub-arrays types for multi-dim.
            let len = dims[0];
            let elem_ty = ty
                .array_item_type()
                .unwrap_or_else(|| panic!("compiler bug: empty array dims"));

            for i in (0..len).rev() {
                if !elem_ty.needs_drop() {
                    continue;
                }

                let idx_op = Operand::Const(Const::Int {
                    signed: false,
                    bits: 64,
                    value: i as i128,
                });
                let proj = vec![Projection::Index { index: idx_op }];
                let elem_ty_id = ty_lowerer.lower_ty(&elem_ty);
                let elem_place = if elem_ty.is_scalar() {
                    PlaceAny::Scalar(Place::new(base, elem_ty_id, proj))
                } else {
                    PlaceAny::Aggregate(Place::new(base, elem_ty_id, proj))
                };
                emit_drop_call(fb, registry, block, &elem_ty, elem_place);
            }
        }

        Type::Enum { variants, .. } => {
            // Only emit switch if any payloads need drop.
            if !variants
                .iter()
                .any(|v| v.payload.iter().any(|p| p.needs_drop()))
            {
                return;
            }

            let ret_block = fb.new_block();

            let tag_ty_id = ty_lowerer.lower_ty(&Type::uint(64));
            let tag_place = Place::new(base, tag_ty_id, vec![Projection::Field { index: 0 }]);

            let mut cases = Vec::new();

            for (idx, variant) in variants.iter().enumerate() {
                if !variant.payload.iter().any(|p| p.needs_drop()) {
                    continue;
                }

                let case_bb = fb.new_block();
                cases.push(SwitchCase {
                    value: idx as u64,
                    target: case_bb,
                });

                let offsets = ty.enum_variant_payload_offsets(&variant.name);

                for ((payload_ty, offset),) in
                    variant.payload.iter().zip(offsets.iter()).map(|p| (p,))
                {
                    if !payload_ty.needs_drop() {
                        continue;
                    }

                    let payload_ty_id = ty_lowerer.lower_ty(payload_ty);
                    let projs = vec![
                        Projection::Field { index: 1 }, // payload blob
                        Projection::ByteOffset { offset: *offset },
                    ];
                    let payload_place = if payload_ty.is_scalar() {
                        PlaceAny::Scalar(Place::new(base, payload_ty_id, projs))
                    } else {
                        PlaceAny::Aggregate(Place::new(base, payload_ty_id, projs))
                    };
                    emit_drop_call(fb, registry, case_bb, payload_ty, payload_place);
                }

                fb.set_terminator(case_bb, Terminator::Goto(ret_block));
            }

            fb.set_terminator(ret_block, Terminator::Return);
            fb.set_terminator(
                block,
                Terminator::Switch {
                    discr: Operand::Copy(tag_place),
                    cases,
                    default: ret_block,
                },
            );
        }

        _ => {}
    }
}

fn emit_drop_call(
    fb: &mut FuncBuilder,
    registry: &mut DropGlueRegistry,
    block: BlockId,
    ty: &Type,
    place: PlaceAny,
) {
    if !ty.needs_drop() {
        return;
    }

    let callee = registry.get_or_create(ty);
    fb.push_stmt(
        block,
        Statement::Call {
            dst: None,
            callee: Callee::Def(callee),
            args: vec![place],
        },
    );
}
