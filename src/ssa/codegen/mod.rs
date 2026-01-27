// SSA-based code generation lives here.

pub mod graph;
pub mod moves;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_moves.rs"]
mod tests_moves;

#[cfg(test)]
#[path = "../../tests/ssa/codegen/t_graph.rs"]
mod tests_graph;
