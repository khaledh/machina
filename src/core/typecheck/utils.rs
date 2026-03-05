//! Shared helper utilities for typecheck phases.

use crate::core::ast::ParamMode;
use crate::core::typecheck::nominal::NominalKey;
use crate::core::types::{FnParamMode, Type};

pub(crate) fn fn_param_mode(mode: ParamMode) -> FnParamMode {
    match mode {
        ParamMode::In => FnParamMode::In,
        ParamMode::InOut => FnParamMode::InOut,
        ParamMode::Out => FnParamMode::Out,
        ParamMode::Sink => FnParamMode::Sink,
    }
}

pub(crate) fn nominal_key_concreteness(key: &NominalKey) -> usize {
    key.type_args
        .iter()
        .filter(|arg| !matches!(arg, Type::Var(_)))
        .count()
}
