//! Closure-related semantic passes.
//!
//! `capture` discovers which outer defs a closure uses and assigns capture
//! modes; `borrow` enforces flow-sensitive exclusivity and escape rules based
//! on those captures.
pub(crate) mod borrow;
pub(crate) mod capture;
