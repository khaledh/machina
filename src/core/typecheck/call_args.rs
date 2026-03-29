//! Named call-argument matching helpers.

use crate::core::ast::ArgLabel;

#[derive(Debug, Clone, PartialEq, Eq)]
pub(crate) enum CallArgMatchError {
    UnknownLabel(String),
    DuplicateParam(String),
    MissingParam(String),
}

pub(crate) fn identity_arg_order(arity: usize) -> Vec<usize> {
    (0..arity).collect()
}

pub(crate) fn match_arg_labels_to_param_names(
    arg_labels: &[Option<ArgLabel>],
    param_names: &[String],
) -> Result<Vec<usize>, CallArgMatchError> {
    if arg_labels.is_empty() {
        return Ok(Vec::new());
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

    if let Some((missing_index, _)) = used.iter().enumerate().find(|(_, used)| !**used) {
        return Err(CallArgMatchError::MissingParam(
            param_names[missing_index].clone(),
        ));
    }

    Ok(arg_order)
}

pub(crate) fn reorder_values_by_arg_order<T: Clone>(values: &[T], arg_order: &[usize]) -> Vec<T> {
    if values.is_empty() {
        return Vec::new();
    }
    if arg_order.is_empty() {
        return values.to_vec();
    }

    let mut ordered = vec![values[0].clone(); values.len()];
    for (arg_index, param_index) in arg_order.iter().copied().enumerate() {
        ordered[param_index] = values[arg_index].clone();
    }
    ordered
}
