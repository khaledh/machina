//! Native support facade for batch build/run paths.
//!
//! The implementation is split across focused driver modules:
//! - `runtime_support`: runtime archive caching
//! - `stdlib_support`: cached stdlib object/archive support
//! - `support_utils`: shared artifact helpers

pub use crate::driver::runtime_support::{ensure_runtime_archive, runtime_source_paths};
pub use crate::driver::stdlib_support::{StdlibArtifacts, ensure_stdlib_archive_for_modules};
pub use crate::driver::support_utils::{
    assemble_object, default_exe_path, temp_named_asm_path, temp_obj_path,
};
