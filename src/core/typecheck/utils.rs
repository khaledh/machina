//! Shared helper utilities for typecheck phases.

use crate::core::typecheck::nominal::NominalKey;
use crate::core::types::{FnParamMode, Type};

pub(crate) fn fn_param_mode(mode: crate::core::tree::ParamMode) -> FnParamMode {
    match mode {
        crate::core::tree::ParamMode::In => FnParamMode::In,
        crate::core::tree::ParamMode::InOut => FnParamMode::InOut,
        crate::core::tree::ParamMode::Out => FnParamMode::Out,
        crate::core::tree::ParamMode::Sink => FnParamMode::Sink,
    }
}

pub(crate) fn nominal_key_concreteness(key: &NominalKey) -> usize {
    key.type_args
        .iter()
        .filter(|arg| !matches!(arg, Type::Var(_)))
        .count()
}
