use crate::driver::project_config::ProjectConfig;
use crate::driver::project_config::ToolKind;
use crate::driver::target::SelectedTarget;

use std::ffi::OsStr;
use std::fs;
use std::fs::OpenOptions;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::thread;
use std::time::Duration;
use std::time::SystemTime;

pub(crate) fn compile_c_object(
    source: &Path,
    object: &Path,
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<(), String> {
    let mut cmd = cc_command_for_target(target, project_config)?;
    let status = cmd
        .arg("-c")
        .arg("-o")
        .arg(object)
        .arg(source)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "cc failed compiling {} with status {}",
            source.display(),
            status
        ))
    }
}

pub(crate) fn archive_objects(
    archive: &Path,
    objects: &[PathBuf],
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<(), String> {
    if archive.exists() {
        match fs::remove_file(archive) {
            Ok(()) => {}
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => {}
            Err(err) => {
                return Err(format!(
                    "failed to remove stale {}: {err}",
                    archive.display()
                ));
            }
        }
    }
    let mut cmd = ar_command_for_target(target, project_config)?;
    let status = cmd
        .arg("rcs")
        .arg(archive)
        .args(objects)
        .status()
        .map_err(|e| format!("failed to invoke ar: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!(
            "ar failed creating {} with status {}",
            archive.display(),
            status
        ))
    }
}

pub fn assemble_object(
    asm_path: &Path,
    obj_path: &Path,
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<(), String> {
    let mut cmd = cc_command_for_target(target, project_config)?;
    let status = cmd
        .arg("-c")
        .arg("-o")
        .arg(obj_path)
        .arg(asm_path)
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
}

pub fn native_toolchain_supports_target(target: crate::backend::TargetKind) -> bool {
    if cfg!(target_os = "macos") {
        return target.macos_cc_arch().is_some();
    }
    target == crate::backend::TargetKind::host()
}

pub(crate) fn supports_configured_toolchain(
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> bool {
    native_toolchain_supports_target(target.kind)
        || project_config.is_some_and(|cfg| {
            cfg.tool(target.config_key(), ToolKind::Cc).is_some()
                && cfg.tool(target.config_key(), ToolKind::Ar).is_some()
        })
}

pub(crate) fn configure_default_cc_for_target(
    cmd: &mut Command,
    target: crate::backend::TargetKind,
) -> Result<(), String> {
    if cfg!(target_os = "macos") {
        let Some(arch) = target.macos_cc_arch() else {
            return Err(format!(
                "native macOS toolchain does not support target {}",
                target.config_key()
            ));
        };
        cmd.arg("-arch").arg(arch);
        return Ok(());
    }

    if native_toolchain_supports_target(target) {
        Ok(())
    } else {
        Err(format!(
            "native toolchain support for target {} is unavailable on host {}",
            target.config_key(),
            crate::backend::TargetKind::host().config_key()
        ))
    }
}

pub(crate) fn cc_command_for_target(
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<Command, String> {
    if let Some(tool) = project_config.and_then(|cfg| cfg.tool(target.config_key(), ToolKind::Cc)) {
        return Ok(tool.to_command());
    }

    let mut cmd = Command::new("cc");
    configure_default_cc_for_target(&mut cmd, target.kind)?;
    Ok(cmd)
}

pub(crate) fn ar_command_for_target(
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<Command, String> {
    if let Some(tool) = project_config.and_then(|cfg| cfg.tool(target.config_key(), ToolKind::Ar)) {
        return Ok(tool.to_command());
    }

    if native_toolchain_supports_target(target.kind) {
        Ok(Command::new("ar"))
    } else {
        Err(format!(
            "no archive tool configured for target {} and native toolchain support is unavailable",
            target.config_key()
        ))
    }
}

pub(crate) fn artifact_is_stale(artifact: &Path, sources: &[PathBuf]) -> Result<bool, String> {
    let Ok(artifact_meta) = fs::metadata(artifact) else {
        return Ok(true);
    };
    let artifact_mtime = artifact_meta.modified().unwrap_or(SystemTime::UNIX_EPOCH);
    for source in sources {
        let source_mtime = fs::metadata(source)
            .and_then(|meta| meta.modified())
            .map_err(|e| format!("failed to stat {}: {e}", source.display()))?;
        if source_mtime > artifact_mtime {
            return Ok(true);
        }
    }
    Ok(false)
}

pub(crate) fn native_support_dir() -> Result<PathBuf, String> {
    let base = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("target"));
    let path = base.join("machina-support");
    fs::create_dir_all(&path).map_err(|e| format!("failed to create {}: {e}", path.display()))?;
    Ok(path)
}

pub(crate) fn with_artifact_lock<T, F>(lock_path: &Path, action: F) -> Result<T, String>
where
    F: FnOnce() -> Result<T, String>,
{
    let _guard = ArtifactLock::acquire(lock_path)?;
    action()
}

struct ArtifactLock {
    path: PathBuf,
}

impl ArtifactLock {
    fn acquire(lock_path: &Path) -> Result<Self, String> {
        if let Some(parent) = lock_path.parent() {
            fs::create_dir_all(parent)
                .map_err(|e| format!("failed to create {}: {e}", parent.display()))?;
        }

        loop {
            match OpenOptions::new()
                .write(true)
                .create_new(true)
                .open(lock_path)
            {
                Ok(_) => {
                    return Ok(Self {
                        path: lock_path.to_path_buf(),
                    });
                }
                Err(err) if err.kind() == std::io::ErrorKind::AlreadyExists => {
                    thread::sleep(Duration::from_millis(10));
                }
                Err(err) => {
                    return Err(format!(
                        "failed to acquire build lock {}: {err}",
                        lock_path.display()
                    ));
                }
            }
        }
    }
}

impl Drop for ArtifactLock {
    fn drop(&mut self) {
        let _ = fs::remove_file(&self.path);
    }
}

pub fn temp_obj_path(name: &str) -> PathBuf {
    let pid = std::process::id();
    let mut path = std::env::temp_dir();
    path.push(format!("machina_{pid}_{name}.o"));
    path
}

pub fn temp_named_asm_path(name: &str) -> PathBuf {
    let pid = std::process::id();
    let mut path = std::env::temp_dir();
    path.push(format!("machina_{pid}_{name}.s"));
    path
}

pub fn default_exe_path(input_path: &Path) -> PathBuf {
    let stem = input_path.file_stem().unwrap_or_else(|| OsStr::new("out"));
    let mut path = input_path.to_path_buf();
    path.set_file_name(stem);
    path
}
