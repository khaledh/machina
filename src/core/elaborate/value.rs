//! Value expression elaboration.
//!
//! This module handles the transformation of normalized expressions (rvalues)
//! into semantic value expressions. The submodules handle specific cases:
//!
//! - `expr`: Main dispatch for value expressions, including calls and closures
//! - `block`: Block items and statements
//! - `string_fmt`: Pre-computing string interpolation plans

#[path = "value_block.rs"]
mod block;
#[path = "value_expr.rs"]
mod expr;
#[path = "value_string_fmt.rs"]
mod string_fmt;
