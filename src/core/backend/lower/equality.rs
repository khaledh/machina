//! Structural equality lowering.
//!
//! This module keeps `==`/`!=` lowering logic out of linear expression lowering
//! by using a small, typed compare plan:
//! 1. Build an `EqPlan` from semantic type shape.
//! 2. Emit IR for that plan.

use crate::backend::lower::lowerer::FuncLowerer;
use crate::ir::{BinOp, Callee, CastKind, CmpOp, IrTypeId, RuntimeFn, ValueId};
use crate::types::{EnumVariant, Type};

#[derive(Clone)]
enum EqPlan {
    /// `unit == unit` is always true.
    AlwaysTrue,

    /// Scalar leaf compare lowered directly to `cmp eq`.
    LeafCmp,

    /// String content compare lowered to runtime helper.
    StringEq,

    /// Tuple compares each field in source order.
    Tuple {
        field_tys: Vec<Type>,
        field_plans: Vec<EqPlan>,
    },

    /// Struct compares each field in declaration order.
    Struct {
        field_tys: Vec<Type>,
        field_plans: Vec<EqPlan>,
    },

    /// Fixed array compares element-by-element over flattened storage order.
    Array {
        elem_ty: Type,
        elem_plan: Box<EqPlan>,
        total_elems: usize,
    },

    /// Enum-like compare (enum and error-union): tag + payload compare.
    Enum { variants: Vec<EnumVariantPlan> },
}

#[derive(Clone)]
struct EnumVariantPlan {
    /// Semantic payload field types for this variant.
    payload_tys: Vec<Type>,

    /// Field-by-field equality plan aligned with `payload_tys`.
    payload_plans: Vec<EqPlan>,
}

impl EqPlan {
    /// Build a type-driven compare plan.
    ///
    /// The plan captures semantic shape only. Backend layout details (tag type,
    /// payload offsets, etc.) are resolved later when emitting IR.
    fn build(ty: &Type) -> Self {
        match ty {
            Type::Unit => Self::AlwaysTrue,
            Type::Int { .. } | Type::Bool | Type::Char | Type::Range { .. } => Self::LeafCmp,
            Type::String => Self::StringEq,
            Type::Tuple { field_tys } => {
                let field_plans = field_tys.iter().map(Self::build).collect();
                Self::Tuple {
                    field_tys: field_tys.clone(),
                    field_plans,
                }
            }
            Type::Struct { fields, .. } => {
                let field_tys = fields
                    .iter()
                    .map(|field| field.ty.clone())
                    .collect::<Vec<_>>();
                let field_plans = field_tys.iter().map(Self::build).collect();
                Self::Struct {
                    field_tys,
                    field_plans,
                }
            }
            Type::Array { elem_ty, dims } => {
                // Arrays compare over all flattened elements.
                let mut total_elems: usize = 1;
                for dim in dims {
                    total_elems = total_elems
                        .checked_mul(*dim)
                        .expect("backend array equality element count overflow");
                }

                let elem_ty = (**elem_ty).clone();
                let elem_plan = Box::new(Self::build(&elem_ty));
                Self::Array {
                    elem_ty,
                    elem_plan,
                    total_elems,
                }
            }
            Type::Enum { variants, .. } => Self::Enum {
                variants: enum_variant_plans(variants),
            },
            Type::ErrorUnion { ok_ty, err_tys } => {
                // Error union is handled like a synthetic enum:
                //   Ok(T) | Err0(E0) | Err1(E1) | ...
                let mut variants = Vec::with_capacity(err_tys.len() + 1);
                variants.push(EnumVariantPlan {
                    payload_tys: vec![(**ok_ty).clone()],
                    payload_plans: vec![Self::build(ok_ty)],
                });
                for err_ty in err_tys {
                    variants.push(EnumVariantPlan {
                        payload_tys: vec![err_ty.clone()],
                        payload_plans: vec![Self::build(err_ty)],
                    });
                }
                Self::Enum { variants }
            }
            other => {
                panic!("backend structural equality unsupported for {:?}", other);
            }
        }
    }
}

/// Convert semantic enum variants into payload plans.
fn enum_variant_plans(variants: &[EnumVariant]) -> Vec<EnumVariantPlan> {
    variants
        .iter()
        .map(|variant| EnumVariantPlan {
            payload_tys: variant.payload.clone(),
            payload_plans: variant.payload.iter().map(EqPlan::build).collect(),
        })
        .collect()
}

impl<'a, 'g> FuncLowerer<'a, 'g> {
    /// Entry point for lowering `lhs == rhs` with semantic type `ty`.
    pub(super) fn lower_eq_value(&mut self, lhs: ValueId, rhs: ValueId, ty: &Type) -> ValueId {
        let plan = EqPlan::build(ty);
        self.emit_eq_with_plan(lhs, rhs, ty, &plan)
    }

    /// Emit equality IR recursively from a pre-built plan.
    fn emit_eq_with_plan(
        &mut self,
        lhs: ValueId,
        rhs: ValueId,
        ty: &Type,
        plan: &EqPlan,
    ) -> ValueId {
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let true_val = self.builder.const_bool(true, bool_ty);

        match plan {
            EqPlan::AlwaysTrue => true_val,
            EqPlan::LeafCmp => self.builder.cmp(CmpOp::Eq, lhs, rhs, bool_ty),
            EqPlan::StringEq => {
                let lhs_addr = self.materialize_value_addr(lhs, ty);
                let rhs_addr = self.materialize_value_addr(rhs, ty);
                self.builder.call(
                    Callee::Runtime(RuntimeFn::StringEq),
                    vec![lhs_addr, rhs_addr],
                    bool_ty,
                )
            }

            EqPlan::Tuple {
                field_tys,
                field_plans,
            } => {
                // Materialize aggregates once, then load each field by index.
                let tuple_ir_ty = self.type_lowerer.lower_type(ty);
                let lhs_slot = self.materialize_value_slot(lhs, tuple_ir_ty);
                let rhs_slot = self.materialize_value_slot(rhs, tuple_ir_ty);

                let mut all_eq = true_val;
                for (index, (field_ty, field_plan)) in
                    field_tys.iter().zip(field_plans.iter()).enumerate()
                {
                    let field_ir_ty = self.type_lowerer.lower_type(field_ty);
                    let lhs_field = self.load_field(lhs_slot.addr, index, field_ir_ty);
                    let rhs_field = self.load_field(rhs_slot.addr, index, field_ir_ty);
                    let field_eq =
                        self.emit_eq_with_plan(lhs_field, rhs_field, field_ty, field_plan);
                    all_eq = self.builder.binop(BinOp::And, all_eq, field_eq, bool_ty);
                }
                all_eq
            }

            EqPlan::Struct {
                field_tys,
                field_plans,
            } => {
                // Declaration-order field compare keeps semantics/layout aligned.
                let struct_ir_ty = self.type_lowerer.lower_type(ty);
                let lhs_slot = self.materialize_value_slot(lhs, struct_ir_ty);
                let rhs_slot = self.materialize_value_slot(rhs, struct_ir_ty);

                let mut all_eq = true_val;
                for (index, (field_ty, field_plan)) in
                    field_tys.iter().zip(field_plans.iter()).enumerate()
                {
                    let field_ir_ty = self.type_lowerer.lower_type(field_ty);
                    let lhs_field = self.load_field(lhs_slot.addr, index, field_ir_ty);
                    let rhs_field = self.load_field(rhs_slot.addr, index, field_ir_ty);
                    let field_eq =
                        self.emit_eq_with_plan(lhs_field, rhs_field, field_ty, field_plan);
                    all_eq = self.builder.binop(BinOp::And, all_eq, field_eq, bool_ty);
                }
                all_eq
            }

            EqPlan::Array {
                elem_ty,
                elem_plan,
                total_elems,
            } => {
                // Arrays are contiguous. Compare each element slot.
                let array_ir_ty = self.type_lowerer.lower_type(ty);
                let lhs_slot = self.materialize_value_slot(lhs, array_ir_ty);
                let rhs_slot = self.materialize_value_slot(rhs, array_ir_ty);
                let elem_ir_ty = self.type_lowerer.lower_type(elem_ty);
                let elem_ptr_ty = self.type_lowerer.ptr_to(elem_ir_ty);
                let u64_ty = self.type_lowerer.lower_type(&Type::uint(64));

                let mut all_eq = true_val;
                for index in 0..*total_elems {
                    let idx = self.builder.const_int(index as i128, false, 64, u64_ty);
                    let lhs_elem_ptr = self.builder.index_addr(lhs_slot.addr, idx, elem_ptr_ty);
                    let rhs_elem_ptr = self.builder.index_addr(rhs_slot.addr, idx, elem_ptr_ty);
                    let lhs_elem = self.builder.load(lhs_elem_ptr, elem_ir_ty);
                    let rhs_elem = self.builder.load(rhs_elem_ptr, elem_ir_ty);
                    let elem_eq = self.emit_eq_with_plan(lhs_elem, rhs_elem, elem_ty, elem_plan);
                    all_eq = self.builder.binop(BinOp::And, all_eq, elem_eq, bool_ty);
                }
                all_eq
            }

            EqPlan::Enum { variants } => self.emit_enum_eq_with_plan(lhs, rhs, ty, variants),
        }
    }

    /// Emit enum/error-union compare:
    /// 1) tags must match
    /// 2) payload for active tag must match
    fn emit_enum_eq_with_plan(
        &mut self,
        lhs: ValueId,
        rhs: ValueId,
        ty: &Type,
        variants: &[EnumVariantPlan],
    ) -> ValueId {
        let bool_ty = self.type_lowerer.lower_type(&Type::Bool);
        let true_val = self.builder.const_bool(true, bool_ty);
        let false_val = self.builder.const_bool(false, bool_ty);

        let enum_ty_id = self
            .type_map
            .type_table()
            .lookup_id(ty)
            .unwrap_or_else(|| panic!("backend enum equality missing type id for {:?}", ty));
        let enum_ir_ty = self.type_lowerer.lower_type(ty);
        let lhs_slot = self.materialize_value_slot(lhs, enum_ir_ty);
        let rhs_slot = self.materialize_value_slot(rhs, enum_ir_ty);

        // Snapshot layout once before recursion. We need tag and payload offsets.
        let (tag_ty, blob_ty, variant_layouts) = {
            let layout = self.type_lowerer.enum_layout(enum_ty_id);
            (layout.tag_ty, layout.blob_ty, layout.variants.clone())
        };

        if variants.len() != variant_layouts.len() {
            panic!(
                "backend enum equality plan/layout mismatch for {:?}: {} vs {}",
                ty,
                variants.len(),
                variant_layouts.len()
            );
        }

        let lhs_tag = self.load_field(lhs_slot.addr, 0, tag_ty);
        let rhs_tag = self.load_field(rhs_slot.addr, 0, tag_ty);
        let tags_eq = self.builder.cmp(CmpOp::Eq, lhs_tag, rhs_tag, bool_ty);

        // Payload bytes are in field #1 (blob).
        let lhs_blob = self.field_addr_typed(lhs_slot.addr, 1, blob_ty);
        let rhs_blob = self.field_addr_typed(rhs_slot.addr, 1, blob_ty);

        // Build: OR over all (is_this_variant AND payload_eq_for_variant)
        let mut any_variant_eq = false_val;
        for (variant_index, (variant_layout, variant_plan)) in
            variant_layouts.iter().zip(variants.iter()).enumerate()
        {
            let tag_const = self
                .builder
                .const_int(variant_layout.tag as i128, false, 32, tag_ty);
            let is_variant = self.builder.cmp(CmpOp::Eq, lhs_tag, tag_const, bool_ty);

            if variant_plan.payload_tys.len() != variant_layout.field_tys.len()
                || variant_plan.payload_tys.len() != variant_layout.field_offsets.len()
                || variant_plan.payload_tys.len() != variant_plan.payload_plans.len()
            {
                panic!(
                    "backend enum equality payload arity mismatch for variant {} of {:?}",
                    variant_index, ty
                );
            }

            let mut payload_eq = true_val;
            for field_index in 0..variant_layout.field_tys.len() {
                // Backend layout drives offsets; semantic plan drives recursive compare type.
                let field_ir_ty = variant_layout.field_tys[field_index];
                let field_offset = variant_layout.field_offsets[field_index];
                let sem_field_ty = &variant_plan.payload_tys[field_index];
                let field_plan = &variant_plan.payload_plans[field_index];

                let lhs_field = self.load_blob_field_value(lhs_blob, field_ir_ty, field_offset);
                let rhs_field = self.load_blob_field_value(rhs_blob, field_ir_ty, field_offset);
                let field_eq =
                    self.emit_eq_with_plan(lhs_field, rhs_field, sem_field_ty, field_plan);
                payload_eq = self
                    .builder
                    .binop(BinOp::And, payload_eq, field_eq, bool_ty);
            }

            let variant_eq = self
                .builder
                .binop(BinOp::And, is_variant, payload_eq, bool_ty);
            any_variant_eq = self
                .builder
                .binop(BinOp::Or, any_variant_eq, variant_eq, bool_ty);
        }

        self.builder
            .binop(BinOp::And, tags_eq, any_variant_eq, bool_ty)
    }

    /// Load one typed field from payload blob at a byte offset.
    fn load_blob_field_value(
        &mut self,
        blob_ptr: ValueId,
        field_ty: IrTypeId,
        field_offset: u64,
    ) -> ValueId {
        let field_bytes = self.byte_offset_addr(blob_ptr, field_offset);
        let field_ptr_ty = self.type_lowerer.ptr_to(field_ty);
        let field_ptr = self
            .builder
            .cast(CastKind::PtrToPtr, field_bytes, field_ptr_ty);
        self.builder.load(field_ptr, field_ty)
    }
}
