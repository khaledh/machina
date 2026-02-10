//! Machina LSP server.
//!
//! This crate intentionally keeps protocol and transport concerns isolated from
//! compiler semantics. The current scope is a minimal JSON-RPC/LSP shell.

pub mod handlers;
pub mod server;
pub mod transport;
