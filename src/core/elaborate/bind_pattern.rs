use crate::core::ast::BindPattern;
use crate::core::elaborate::elaborator::Elaborator;
use crate::core::types::Type;

impl<'a> Elaborator<'a> {
    pub(super) fn elab_bind_pattern(&mut self, pattern: &BindPattern, _ty: &Type) -> BindPattern {
        pattern.clone()
    }
}
