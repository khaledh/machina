//! SSA analysis utilities.

pub mod cfg;
pub mod liveness;

#[cfg(test)]
#[path = "../../tests/ssa/analysis/t_cfg.rs"]
mod t_cfg;
#[cfg(test)]
#[path = "../../tests/ssa/analysis/t_liveness.rs"]
mod t_liveness;
