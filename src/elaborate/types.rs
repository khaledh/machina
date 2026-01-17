use crate::diag::Span;
use crate::tree::ParamMode;
use crate::tree::semantic as sem;
use crate::types::Type;

use super::elaborator::Elaborator;

impl<'a> Elaborator<'a> {
    pub(super) fn type_expr_from_type(&mut self, ty: &Type, span: Span) -> sem::TypeExpr {
        let id = self.node_id_gen.new_id();
        // Build a semantic TypeExpr tree for synthesized defs (e.g. closure fields).
        let kind = match ty {
            Type::Unknown => {
                panic!("compiler bug: unknown type in closure capture at {}", span)
            }
            Type::Unit => self.named_type_expr("()", span),
            Type::Int { signed, bits } => {
                let name = match (*signed, *bits) {
                    (false, 8) => "u8",
                    (false, 16) => "u16",
                    (false, 32) => "u32",
                    (false, 64) => "u64",
                    (true, 8) => "i8",
                    (true, 16) => "i16",
                    (true, 32) => "i32",
                    (true, 64) => "i64",
                    _ => {
                        panic!("compiler bug: unsupported int type signed={signed} bits={bits}")
                    }
                };
                self.named_type_expr(name, span)
            }
            Type::Bool => self.named_type_expr("bool", span),
            Type::Char => self.named_type_expr("char", span),
            Type::String => self.named_type_expr("string", span),
            Type::Range { min, max } => sem::TypeExprKind::Range {
                min: *min,
                max: *max,
            },
            Type::Array { elem_ty, dims } => sem::TypeExprKind::Array {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
                dims: dims.clone(),
            },
            Type::Tuple { field_tys } => sem::TypeExprKind::Tuple {
                field_ty_exprs: field_tys
                    .iter()
                    .map(|field| self.type_expr_from_type(field, span))
                    .collect(),
            },
            Type::Struct { name, .. } | Type::Enum { name, .. } => self.named_type_expr(name, span),
            Type::Slice { elem_ty } => sem::TypeExprKind::Slice {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Heap { elem_ty } => sem::TypeExprKind::Heap {
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Ref { mutable, elem_ty } => sem::TypeExprKind::Ref {
                mutable: *mutable,
                elem_ty_expr: Box::new(self.type_expr_from_type(elem_ty, span)),
            },
            Type::Fn { params, ret_ty } => sem::TypeExprKind::Fn {
                params: params
                    .iter()
                    .map(|param| sem::FnTypeParam {
                        mode: match param.mode {
                            crate::types::FnParamMode::In => ParamMode::In,
                            crate::types::FnParamMode::InOut => ParamMode::InOut,
                            crate::types::FnParamMode::Out => ParamMode::Out,
                            crate::types::FnParamMode::Sink => ParamMode::Sink,
                        },
                        ty_expr: self.type_expr_from_type(&param.ty, span),
                    })
                    .collect(),
                ret_ty_expr: Box::new(self.type_expr_from_type(ret_ty, span)),
            },
        };

        sem::TypeExpr { id, kind, span }
    }

    fn named_type_expr(&self, name: &str, span: Span) -> sem::TypeExprKind {
        // Resolve a builtin/nominal type name into its DefId.
        let def_id = self
            .def_table
            .lookup_type_def_id(name)
            .unwrap_or_else(|| panic!("compiler bug: missing def id for type {name} at {}", span));
        sem::TypeExprKind::Named {
            ident: name.to_string(),
            def_id,
        }
    }
}
