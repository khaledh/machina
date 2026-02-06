//! Error surface for the new type checker.
//!
//! For migration safety, reuse the legacy error types verbatim.

pub use crate::typeck::{TypeCheckError, TypeCheckErrorKind};
