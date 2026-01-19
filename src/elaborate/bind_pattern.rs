use crate::elaborate::elaborator::Elaborator;
use crate::tree::normalized as norm;
use crate::tree::semantic as sem;
use crate::types::Type;

impl<'a> Elaborator<'a> {
    pub(super) fn elab_bind_pattern(
        &mut self,
        pattern: &norm::BindPattern,
        ty: &Type,
    ) -> sem::BindPattern {
        let kind = match &pattern.kind {
            norm::BindPatternKind::Name { ident, def_id } => sem::BindPatternKind::Name {
                ident: ident.clone(),
                def_id: *def_id,
            },
            norm::BindPatternKind::Array { patterns } => {
                let elem_ty = ty
                    .array_item_type()
                    .unwrap_or_else(|| panic!("compiler bug: array pattern on non-array type"));
                let patterns = patterns
                    .iter()
                    .map(|pattern| self.elab_bind_pattern(pattern, &elem_ty))
                    .collect();
                sem::BindPatternKind::Array { patterns }
            }
            norm::BindPatternKind::Tuple { patterns } => {
                let Type::Tuple { field_tys } = ty else {
                    panic!("compiler bug: tuple pattern on non-tuple type");
                };
                if patterns.len() != field_tys.len() {
                    panic!("compiler bug: tuple pattern arity mismatch");
                }

                let fields = patterns
                    .iter()
                    .zip(field_tys.iter())
                    .enumerate()
                    .map(|(index, (pattern, field_ty))| sem::TupleFieldBindPattern {
                        index,
                        pattern: self.elab_bind_pattern(pattern, field_ty),
                    })
                    .collect();
                sem::BindPatternKind::Tuple { fields }
            }
            norm::BindPatternKind::Struct { name, fields } => {
                let Type::Struct {
                    fields: struct_fields,
                    ..
                } = ty
                else {
                    panic!("compiler bug: struct pattern on non-struct type");
                };

                let fields = fields
                    .iter()
                    .map(|field| {
                        let field_index = ty.struct_field_index(&field.name);
                        let field_ty = struct_fields
                            .get(field_index)
                            .map(|field| field.ty.clone())
                            .unwrap_or_else(|| {
                                panic!("compiler bug: missing struct field index {field_index}")
                            });
                        sem::StructFieldBindPattern {
                            name: field.name.clone(),
                            field_index,
                            pattern: self.elab_bind_pattern(&field.pattern, &field_ty),
                            span: field.span,
                        }
                    })
                    .collect();

                sem::BindPatternKind::Struct {
                    name: name.clone(),
                    fields,
                }
            }
        };

        sem::BindPattern {
            id: pattern.id,
            kind,
            span: pattern.span,
        }
    }
}
