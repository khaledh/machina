//! HIR folder re-exports (currently identical to AST folders).

pub use crate::ast::fold::{
    AstFolder, walk_array_lit_init, walk_binary_expr, walk_block, walk_block_item, walk_block_tail,
    walk_call, walk_call_args, walk_closure, walk_expr, walk_exprs, walk_func, walk_if,
    walk_match_arm, walk_match_arms, walk_method, walk_method_block, walk_module,
};
