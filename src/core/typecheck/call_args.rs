//! Named call-argument matching helpers.

use crate::core::ast::ArgLabel;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallArgMatchError {
    UnknownLabel(String),
    DuplicateParam(String),
    MissingParam(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) struct CallArgMatch {
    pub(crate) arg_order: Vec<usize>,
    pub(crate) missing_params: Vec<usize>,
}

pub(crate) fn identity_arg_order(arity: usize) -> Vec<usize> {
    (0..arity).collect()
}

pub(crate) fn match_arg_labels_to_param_names(
    arg_labels: &[Option<ArgLabel>],
    param_names: &[String],
    has_default: &[bool],
) -> Result<CallArgMatch, CallArgMatchError> {
    if arg_labels.is_empty() {
        if arg_labels.len() > param_names.len() {
            return Err(CallArgMatchError::MissingParam(
                param_names.last().cloned().unwrap_or_default(),
            ));
        }
        let missing_params = (arg_labels.len()..param_names.len())
            .filter(|index| has_default.get(*index).copied().unwrap_or(false))
            .collect::<Vec<_>>();
        if let Some(missing_index) = (arg_labels.len()..param_names.len())
            .find(|index| !has_default.get(*index).copied().unwrap_or(false))
        {
            return Err(CallArgMatchError::MissingParam(
                param_names[missing_index].clone(),
            ));
        }
        return Ok(CallArgMatch {
            arg_order: identity_arg_order(arg_labels.len()),
            missing_params,
        });
    }

    let mut next_positional = 0usize;
    let mut used = vec![false; param_names.len()];
    let mut arg_order = Vec::with_capacity(arg_labels.len());

    for label in arg_labels {
        let param_index = if let Some(label) = label {
            let Some(index) = param_names.iter().position(|name| name == &label.name) else {
                return Err(CallArgMatchError::UnknownLabel(label.name.clone()));
            };
            if used[index] {
                return Err(CallArgMatchError::DuplicateParam(label.name.clone()));
            }
            index
        } else {
            let index = next_positional;
            next_positional += 1;
            index
        };

        if param_index >= param_names.len() {
            return Err(CallArgMatchError::MissingParam(
                param_names.last().cloned().unwrap_or_default(),
            ));
        }
        used[param_index] = true;
        arg_order.push(param_index);
    }

    let mut missing_params = Vec::new();
    for (index, assigned) in used.iter().enumerate() {
        if *assigned {
            continue;
        }
        if has_default.get(index).copied().unwrap_or(false) {
            missing_params.push(index);
            continue;
        }
        return Err(CallArgMatchError::MissingParam(param_names[index].clone()));
    }

    Ok(CallArgMatch {
        arg_order,
        missing_params,
    })
}
