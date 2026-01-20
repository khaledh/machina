use crate::lower::errors::LowerError;
use crate::lower::lower_ast::{FuncLowerer, PlaceKind};
use crate::mcir::types::*;
use crate::tree::NodeId;
use crate::tree::semantic::{
    IndexBaseKind, IndexPlan, PlaceExpr, PlaceExprKind as PEK, SliceBaseKind, SlicePlan,
    TypeDefKind, ValueExpr,
};
use crate::typeck::type_map::resolve_type_expr;
use crate::types::{StructField as TypeStructField, Type};

impl<'a> FuncLowerer<'a> {
    // --- Place (Lvalue) ---

    /// Lower a SIR place into a MCIR place.
    pub(super) fn lower_place(&mut self, place: &PlaceExpr) -> Result<PlaceAny, LowerError> {
        match &place.kind {
            PEK::Var { .. } => self.lower_var(place),
            PEK::Deref { value } => self.lower_deref(place, value),
            PEK::ArrayIndex { target, indices } => self.lower_array_index(place, target, indices),
            PEK::TupleField { target, index } => self.lower_tuple_field(target, *index),
            PEK::StructField { target, field } => self.lower_struct_field(target, field),
        }
    }

    /// Lower an lvalue expression expected to be scalar.
    pub(super) fn lower_place_scalar(
        &mut self,
        place: &PlaceExpr,
    ) -> Result<Place<Scalar>, LowerError> {
        match self.lower_place(place)? {
            PlaceAny::Scalar(p) => Ok(p),
            PlaceAny::Aggregate(_) => Err(LowerError::PlaceKindMismatch {
                node_id: place.id,
                expected: PlaceKind::Scalar,
            }),
        }
    }

    /// Lower an lvalue expression expected to be aggregate.
    pub(super) fn lower_place_agg(
        &mut self,
        place: &PlaceExpr,
    ) -> Result<Place<Aggregate>, LowerError> {
        match self.lower_place(place)? {
            PlaceAny::Scalar(_) => Err(LowerError::PlaceKindMismatch {
                node_id: place.id,
                expected: PlaceKind::Aggregate,
            }),
            PlaceAny::Aggregate(p) => Ok(p),
        }
    }

    /// Resolve a variable reference into its local place.
    pub(super) fn lower_var(&mut self, place: &PlaceExpr) -> Result<PlaceAny, LowerError> {
        let PEK::Var { def_id, .. } = place.kind else {
            return Err(LowerError::ExprIsNotPlace(place.id));
        };
        let local_id = *self
            .locals
            .get(&def_id)
            .ok_or(LowerError::VarLocalNotFound(place.id, def_id))?;

        let local_ty = self.fb.body.locals[local_id.0 as usize].ty;

        let place = self.place_from_ty_id(local_id, local_ty, vec![]);

        Ok(place)
    }

    /// Lower array indexing into a projected place.
    pub(super) fn lower_array_index(
        &mut self,
        place: &PlaceExpr,
        target: &PlaceExpr,
        indices: &[ValueExpr],
    ) -> Result<PlaceAny, LowerError> {
        let plan = self.index_plan_for(place)?;
        match plan.base {
            IndexBaseKind::Array { dims, deref_count } => {
                self.lower_array_index_with_plan(place, target, indices, &dims, deref_count)
            }
            IndexBaseKind::Slice { deref_count } => {
                self.lower_slice_index_with_plan(place, target, indices, deref_count)
            }
            IndexBaseKind::String { .. } => Err(LowerError::UnsupportedOperandExpr(place.id)),
        }
    }

    fn lower_array_index_with_plan(
        &mut self,
        place: &PlaceExpr,
        target: &PlaceExpr,
        indices: &[ValueExpr],
        dims: &[u64],
        deref_count: usize,
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place(target)?;
        if indices.len() > dims.len() {
            panic!("compiler bug: too many indices for array (type checker should catch this)");
        }

        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        // Lower each index expression to an operand + emit bounds checks.
        let mut index_operands = Vec::with_capacity(indices.len());
        for (idx_expr, dim) in indices.iter().zip(dims) {
            let idx_op = self.lower_scalar_expr(idx_expr)?;
            let dim_op = Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: *dim as i128,
            });

            let cond_op = self.emit_scalar_rvalue(
                bool_ty_id,
                Rvalue::BinOp {
                    op: BinOp::Lt,
                    lhs: idx_op.clone(),
                    rhs: dim_op.clone(),
                },
            );
            self.emit_runtime_check(
                cond_op,
                CheckKind::Bounds {
                    index: idx_op.clone(),
                    len: dim_op,
                },
            );

            index_operands.push(idx_op);
        }

        let result_ty = self.ty_from_id(place.ty);
        let result_ty_id = self.ty_lowerer.lower_ty(&result_ty);

        let (base, mut projs) = match target_place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            projs.push(Projection::Deref);
        }
        projs.extend(
            index_operands
                .into_iter()
                .map(|index_place| Projection::Index { index: index_place }),
        );

        let place = self.place_from_ty_id(base, result_ty_id, projs);
        Ok(place)
    }

    fn lower_deref(
        &mut self,
        place: &PlaceExpr,
        value: &ValueExpr,
    ) -> Result<PlaceAny, LowerError> {
        let ptr_op = self.lower_scalar_expr(value)?;
        let ptr_place = match ptr_op {
            Operand::Copy(place) | Operand::Move(place) => place,
            operand => {
                let ptr_ty = self.ty_from_id(value.ty);
                let ptr_ty_id = self.ty_lowerer.lower_ty(&ptr_ty);
                let temp = self.new_temp_scalar(ptr_ty_id);
                self.emit_copy_scalar(temp.clone(), Rvalue::Use(operand));
                temp
            }
        };

        let elem_ty = self.ty_from_id(place.ty);
        let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);

        let mut projs = ptr_place.projections().to_vec();
        projs.push(Projection::Deref);
        Ok(self.place_from_ty_id(ptr_place.base(), elem_ty_id, projs))
    }

    /// Lower slice indexing into a projected place.
    pub(super) fn lower_slice_index_with_plan(
        &mut self,
        place: &PlaceExpr,
        target: &PlaceExpr,
        indices: &[ValueExpr],
        deref_count: usize,
    ) -> Result<PlaceAny, LowerError> {
        if indices.len() != 1 {
            return Err(LowerError::UnsupportedOperandExpr(place.id));
        }

        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let bool_ty_id = self.ty_lowerer.lower_ty(&Type::Bool);

        let target_place = self.lower_place(target)?;

        let (base, mut base_projs) = match target_place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            base_projs.push(Projection::Deref);
        }

        let mut ptr_proj = base_projs.clone();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_place = Place::new(base, u64_ty_id, ptr_proj);
        let ptr_op = Operand::Copy(ptr_place);

        let mut len_proj = base_projs;
        len_proj.push(Projection::Field { index: 1 });
        let len_place = Place::new(base, u64_ty_id, len_proj);
        let len_op = Operand::Copy(len_place);

        let index_op = self.lower_scalar_expr(&indices[0])?;
        let cond_op = self.emit_scalar_rvalue(
            bool_ty_id,
            Rvalue::BinOp {
                op: BinOp::Lt,
                lhs: index_op.clone(),
                rhs: len_op.clone(),
            },
        );
        self.emit_runtime_check(
            cond_op,
            CheckKind::Bounds {
                index: index_op.clone(),
                len: len_op.clone(),
            },
        );

        // Treat the slice pointer as a raw pointer to the element type.
        let elem_ty = self.ty_from_id(place.ty);
        let ptr_ty_id = self.ty_lowerer.lower_ty(&Type::Heap {
            elem_ty: Box::new(elem_ty.clone()),
        });
        let ptr_temp = self.new_temp_scalar(ptr_ty_id);
        self.emit_copy_scalar(ptr_temp.clone(), Rvalue::Use(ptr_op));

        let elem_ty_id = self.ty_lowerer.lower_ty(&elem_ty);
        Ok(self.place_from_ty_id(
            ptr_temp.base(),
            elem_ty_id,
            vec![Projection::Index { index: index_op }],
        ))
    }

    /// Lower slice into a projected place.
    pub(super) fn lower_slice_into(
        &mut self,
        dst: &Place<Aggregate>,
        expr_id: NodeId,
        target: &PlaceExpr,
        start: &Option<Box<ValueExpr>>,
        end: &Option<Box<ValueExpr>>,
    ) -> Result<(), LowerError> {
        let u64_ty_id = self.ty_lowerer.lower_ty(&Type::uint(64));
        let plan = self.slice_plan_for(expr_id)?;

        let (base_ptr_op, base_len_op) = match plan.base {
            SliceBaseKind::Array { len, deref_count } => {
                let target_place = self.lower_place_agg(target)?;
                let (base, mut projs) = (target_place.base(), target_place.projections().to_vec());
                for _ in 0..deref_count {
                    projs.push(Projection::Deref);
                }

                let base_ptr_place = self.new_temp_scalar(u64_ty_id);
                self.emit_copy_scalar(
                    base_ptr_place.clone(),
                    Rvalue::AddrOf(PlaceAny::Aggregate(Place::new(
                        base,
                        target_place.ty(),
                        projs,
                    ))),
                );
                let base_ptr_op = Operand::Copy(base_ptr_place);
                let base_len_op = Operand::Const(Const::Int {
                    signed: false,
                    bits: 64,
                    value: len as i128,
                });
                (base_ptr_op, base_len_op)
            }
            SliceBaseKind::Slice { deref_count } => {
                let target_place = self.lower_place_agg(target)?;
                let (base, mut base_projs) =
                    (target_place.base(), target_place.projections().to_vec());
                for _ in 0..deref_count {
                    base_projs.push(Projection::Deref);
                }

                let mut ptr_proj = base_projs.clone();
                ptr_proj.push(Projection::Field { index: 0 });
                let ptr_place = Place::new(base, u64_ty_id, ptr_proj);
                let base_ptr_op = Operand::Copy(ptr_place);

                let mut len_proj = base_projs;
                len_proj.push(Projection::Field { index: 1 });
                let len_place = Place::new(base, u64_ty_id, len_proj);
                let base_len_op = Operand::Copy(len_place);

                (base_ptr_op, base_len_op)
            }
            SliceBaseKind::String { deref_count } => {
                let target_place = self.lower_place_agg(target)?;
                let (base, mut base_projs) =
                    (target_place.base(), target_place.projections().to_vec());
                for _ in 0..deref_count {
                    base_projs.push(Projection::Deref);
                }

                let mut ptr_proj = base_projs.clone();
                ptr_proj.push(Projection::Field { index: 0 });
                let ptr_place = Place::new(base, u64_ty_id, ptr_proj);
                let base_ptr_op = Operand::Copy(ptr_place);

                let u32_ty_id = self.ty_lowerer.lower_ty(&Type::uint(32));
                let mut len_proj = base_projs;
                len_proj.push(Projection::Field { index: 1 });
                let len_place = Place::new(base, u32_ty_id, len_proj);
                let len_u32_op = Operand::Copy(len_place);
                let base_len_op = self.emit_scalar_rvalue(u64_ty_id, Rvalue::Use(len_u32_op));

                (base_ptr_op, base_len_op)
            }
        };

        // start/end operands
        let start_op = match start {
            Some(expr) => self.lower_scalar_expr(expr)?,
            None => Operand::Const(Const::Int {
                signed: false,
                bits: 64,
                value: 0_i128,
            }),
        };

        let end_op = match end {
            Some(expr) => self.lower_scalar_expr(expr)?,
            None => base_len_op.clone(),
        };

        // offset = start * elem_size
        let elem_size = plan.elem_size as i128;
        let elem_size_op = Operand::Const(Const::Int {
            signed: false,
            bits: 64,
            value: elem_size,
        });
        let offset_op = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Mul,
                lhs: start_op.clone(),
                rhs: elem_size_op.clone(),
            },
        );

        // ptr = base_ptr + offset
        let ptr_at = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Add,
                lhs: base_ptr_op,
                rhs: offset_op,
            },
        );

        // len = end - start
        let len_at = self.emit_scalar_rvalue(
            u64_ty_id,
            Rvalue::BinOp {
                op: BinOp::Sub,
                lhs: end_op,
                rhs: start_op,
            },
        );

        // write { ptr, len } into dst
        let mut ptr_proj = dst.projections().to_vec();
        ptr_proj.push(Projection::Field { index: 0 });
        let ptr_field = Place::new(dst.base(), u64_ty_id, ptr_proj);

        let mut len_proj = dst.projections().to_vec();
        len_proj.push(Projection::Field { index: 1 });
        let len_field = Place::new(dst.base(), u64_ty_id, len_proj);

        self.emit_copy_scalar(ptr_field, Rvalue::Use(ptr_at));
        self.emit_copy_scalar(len_field, Rvalue::Use(len_at));

        Ok(())
    }

    pub(super) fn index_plan_for(&self, place: &PlaceExpr) -> Result<IndexPlan, LowerError> {
        self.ctx
            .type_map
            .lookup_index_plan(place.id)
            .ok_or_else(|| LowerError::UnsupportedOperandExpr(place.id))
    }

    pub(super) fn slice_plan_for(&self, node_id: NodeId) -> Result<SlicePlan, LowerError> {
        self.ctx
            .type_map
            .lookup_slice_plan(node_id)
            .ok_or_else(|| LowerError::UnsupportedOperandExpr(node_id))
    }

    /// Lower tuple field access into a projected place.
    pub(super) fn lower_tuple_field(
        &mut self,
        target: &PlaceExpr,
        index: usize,
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place(target)?;
        let target_ty = self.ty_from_id(target.ty);
        let (peeled_ty, deref_count) = target_ty.peel_heap_with_count();

        let field_ty = peeled_ty.tuple_field_type(index);
        let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);

        let (base, mut projs) = match target_place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            projs.push(Projection::Deref);
        }
        projs.push(Projection::Field { index });

        let place = self.place_from_ty_id(base, field_ty_id, projs);

        Ok(place)
    }

    /// Lower struct field access into a projected place.
    pub(super) fn lower_struct_field(
        &mut self,
        target: &PlaceExpr,
        field_name: &str,
    ) -> Result<PlaceAny, LowerError> {
        let target_place = self.lower_place(target)?;
        let target_ty = self.ty_from_id(target.ty);
        let (peeled_ty, deref_count) = target_ty.peel_heap_with_count();
        let peeled_ty = self.expand_shallow_struct(&peeled_ty);

        let field_ty = peeled_ty.struct_field_type(field_name);
        let field_ty_id = self.ty_lowerer.lower_ty(&field_ty);
        // Map field name to index
        let field_index = peeled_ty.struct_field_index(field_name);

        let (base, mut projs) = match target_place {
            PlaceAny::Scalar(p) => (p.base(), p.projections().to_vec()),
            PlaceAny::Aggregate(p) => (p.base(), p.projections().to_vec()),
        };
        for _ in 0..deref_count {
            projs.push(Projection::Deref);
        }
        projs.push(Projection::Field { index: field_index });

        let place = self.place_from_ty_id(base, field_ty_id, projs);

        Ok(place)
    }

    fn expand_shallow_struct(&self, ty: &Type) -> Type {
        let Type::Struct { name, fields } = ty else {
            return ty.clone();
        };
        if !fields.is_empty() {
            return ty.clone();
        }

        let defs = self.ctx.module.type_defs();
        let def = defs.iter().find(|def| def.name == *name);
        let Some(def) = def else {
            return ty.clone();
        };
        let TypeDefKind::Struct { fields } = &def.kind else {
            return ty.clone();
        };

        let resolved_fields = fields
            .iter()
            .filter_map(|f| {
                let field_ty =
                    resolve_type_expr(&self.ctx.def_table, &self.ctx.module, &f.ty).ok()?;
                Some(TypeStructField {
                    name: f.name.clone(),
                    ty: field_ty,
                })
            })
            .collect::<Vec<_>>();

        Type::Struct {
            name: name.clone(),
            fields: resolved_fields,
        }
    }

    /// Create a projected place from an aggregate base.
    pub(super) fn project_place(
        &self,
        src: &Place<Aggregate>,
        proj: Projection,
        ty_id: TyId,
    ) -> PlaceAny {
        let mut projs = src.projections().to_vec();
        projs.push(proj);
        self.place_from_ty_id(src.base(), ty_id, projs)
    }

    /// Construct a typed place from a base local and projections.
    pub(super) fn place_from_ty_id(
        &self,
        base: LocalId,
        ty: TyId,
        proj: Vec<Projection>,
    ) -> PlaceAny {
        if self.is_scalar(ty) {
            PlaceAny::Scalar(Place::new(base, ty, proj))
        } else {
            PlaceAny::Aggregate(Place::new(base, ty, proj))
        }
    }
}
