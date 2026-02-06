use super::*;

impl TypeChecker {
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
