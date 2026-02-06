use super::*;

impl TypeChecker {
    pub(super) fn err_unknown_type(&self, span: Span) -> TypeCheckError {
        TypeCheckErrorKind::UnknownType(span).into()
    }

    pub(super) fn err_arg_count_mismatch(
        &self,
        name: impl Into<String>,
        expected: usize,
        found: usize,
        span: Span,
    ) -> TypeCheckError {
        TypeCheckErrorKind::ArgCountMismatch(name.into(), expected, found, span).into()
    }

    pub(super) fn err_invalid_struct_field_target(&self, ty: Type, span: Span) -> TypeCheckError {
        TypeCheckErrorKind::InvalidStructFieldTarget(ty, span).into()
    }

    pub(super) fn err_invalid_index_target_type(&self, ty: Type, span: Span) -> TypeCheckError {
        TypeCheckErrorKind::InvalidIndexTargetType(ty, span).into()
    }

    pub(super) fn err_index_type_not_int(&self, ty: Type, span: Span) -> TypeCheckError {
        TypeCheckErrorKind::IndexTypeNotInt(ty, span).into()
    }

    pub(super) fn err_len_target_not_lvalue(&self, span: Span) -> TypeCheckError {
        TypeCheckErrorKind::LenTargetNotLvalue(span).into()
    }

    pub(super) fn err_property_not_readable(
        &self,
        field: impl Into<String>,
        span: Span,
    ) -> TypeCheckError {
        TypeCheckErrorKind::PropertyNotReadable(field.into(), span).into()
    }

    pub(super) fn err_property_not_writable(
        &self,
        field: impl Into<String>,
        span: Span,
    ) -> TypeCheckError {
        TypeCheckErrorKind::PropertyNotWritable(field.into(), span).into()
    }
}
