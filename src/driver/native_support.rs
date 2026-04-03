//! Native support facade for batch build/run paths.
//!
//! The implementation is split across focused driver modules:
//! - `runtime_support`: runtime archive caching
//! - `stdlib_support`: cached stdlib object/archive support
//! - `support_utils`: shared artifact helpers

use crate::backend::TargetKind;
use crate::driver::project_config::ProjectConfig;
use crate::driver::support_utils::cc_command_for_target;
use std::path::{Path, PathBuf};

pub use crate::driver::runtime_support::{ensure_runtime_archive, runtime_source_paths};
pub use crate::driver::stdlib_support::{StdlibArtifacts, ensure_stdlib_archive_for_modules};
pub use crate::driver::support_utils::{
    assemble_object, default_exe_path, native_toolchain_supports_target, temp_named_asm_path,
    temp_obj_path,
};

pub fn link_executable(
    asm_path: &Path,
    extra_objs: &[PathBuf],
    exe_path: &Path,
    target: TargetKind,
    project_config: Option<&ProjectConfig>,
) -> Result<(), String> {
    let runtime_archive = ensure_runtime_archive(target, project_config)?;
    let mut cmd = cc_command_for_target(target, project_config)?;
    let status = cmd
        .arg("-o")
        .arg(exe_path)
        .arg(asm_path)
        .args(extra_objs)
        .arg(runtime_archive)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
}
