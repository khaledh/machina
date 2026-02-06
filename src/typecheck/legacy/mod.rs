mod checker;
mod overloads;

pub(crate) mod errors {
    pub(crate) use crate::typecheck::{TypeCheckError, TypeCheckErrorKind};
}

pub(crate) mod type_map {
    pub(crate) use crate::typecheck::type_map::*;
}

pub(crate) mod unify {
    pub(crate) use crate::typecheck::Unifier;
}

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::tree::typed::build_module;
use crate::typecheck::TypeCheckError;
use crate::typecheck::legacy::checker::TypeChecker;

pub(crate) fn type_check_legacy(
    context: ResolvedContext,
) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    let mut type_checker = TypeChecker::new(context.clone());
    let (type_map, call_sigs, generic_insts) = type_checker.check()?;
    let typed_module = build_module(&type_map, &context.module);
    let type_checked_context =
        context.with_type_map(type_map, call_sigs, generic_insts, typed_module);
    Ok(type_checked_context)
}
