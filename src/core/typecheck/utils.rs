//! Shared helper utilities for typecheck phases.

use crate::typecheck::nominal::NominalKey;
use crate::types::{FnParamMode, Type};

pub(crate) fn fn_param_mode(mode: crate::tree::ParamMode) -> FnParamMode {
    match mode {
        crate::tree::ParamMode::In => FnParamMode::In,
        crate::tree::ParamMode::InOut => FnParamMode::InOut,
        crate::tree::ParamMode::Out => FnParamMode::Out,
        crate::tree::ParamMode::Sink => FnParamMode::Sink,
    }
}

pub(crate) fn nominal_key_concreteness(key: &NominalKey) -> usize {
    key.type_args
        .iter()
        .filter(|arg| !matches!(arg, Type::Var(_)))
        .count()
}
