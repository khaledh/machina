//! HIR visitor re-exports (currently identical to AST visitors).

pub use crate::ast::{
    Visitor, walk_block_item, walk_enum_variant, walk_enum_variants, walk_expr, walk_func,
    walk_func_decl, walk_func_sig, walk_method, walk_method_block, walk_method_sig, walk_module,
    walk_param, walk_stmt_expr, walk_struct_field, walk_struct_fields, walk_type_decl,
    walk_type_expr,
};
