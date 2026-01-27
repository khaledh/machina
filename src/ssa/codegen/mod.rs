// SSA-based code generation lives here.

pub mod arm64;
pub mod emitter;
pub mod graph;
pub mod moves;
pub mod traverse;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_moves.rs"]
mod tests_moves;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_graph.rs"]
mod tests_graph;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_traverse.rs"]
mod tests_traverse;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_emitter.rs"]
mod tests_emitter;
