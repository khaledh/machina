pub mod type_map;

mod checker;
mod errors;
mod overloads;
mod unify;

pub use errors::{TypeCheckError, TypeCheckErrorKind};
pub use unify::{Unifier, UnifyError};

use crate::context::{ResolvedContext, TypeCheckedContext};
use crate::tree::typed::build_module;
use crate::typeck::checker::TypeChecker;

pub fn type_check(context: ResolvedContext) -> Result<TypeCheckedContext, Vec<TypeCheckError>> {
    if use_rewrite_typecheck() {
        return crate::typecheck::type_check(context);
    }
    type_check_legacy(context)
}

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

fn use_rewrite_typecheck() -> bool {
    if let Ok(backend) = std::env::var("MACHINA_TYPECHECK_BACKEND") {
        return parse_backend_env(&backend) == Backend::Rewrite;
    }
    if let Ok(flag) = std::env::var("MACHINA_TYPECHECK_REWRITE") {
        return parse_bool_like(&flag);
    }
    false
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Backend {
    Legacy,
    Rewrite,
}

fn parse_backend_env(value: &str) -> Backend {
    match value.trim().to_ascii_lowercase().as_str() {
        "rewrite" | "new" | "typecheck" => Backend::Rewrite,
        _ => Backend::Legacy,
    }
}

fn parse_bool_like(value: &str) -> bool {
    matches!(
        value.trim().to_ascii_lowercase().as_str(),
        "1" | "true" | "yes" | "on" | "rewrite"
    )
}

#[cfg(test)]
mod backend_parse_tests {
    use super::*;

    #[test]
    fn test_parse_backend_env() {
        assert_eq!(parse_backend_env("rewrite"), Backend::Rewrite);
        assert_eq!(parse_backend_env("new"), Backend::Rewrite);
        assert_eq!(parse_backend_env("typecheck"), Backend::Rewrite);
        assert_eq!(parse_backend_env("legacy"), Backend::Legacy);
        assert_eq!(parse_backend_env("something-else"), Backend::Legacy);
    }

    #[test]
    fn test_parse_bool_like() {
        assert!(parse_bool_like("1"));
        assert!(parse_bool_like("true"));
        assert!(parse_bool_like("yes"));
        assert!(parse_bool_like("on"));
        assert!(parse_bool_like("rewrite"));
        assert!(!parse_bool_like("0"));
        assert!(!parse_bool_like("false"));
        assert!(!parse_bool_like("legacy"));
    }
}

#[cfg(test)]
#[path = "../tests/typeck/t_typeck.rs"]
mod tests;

#[cfg(test)]
#[path = "../tests/typeck/t_unify.rs"]
mod tests_unify;
