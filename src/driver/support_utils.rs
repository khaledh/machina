use crate::backend::TargetKind;

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
    target: TargetKind,
) -> Result<(), String> {
    let mut cmd = Command::new("cc");
    configure_cc_for_target(&mut cmd, target)?;
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

pub(crate) fn archive_objects(archive: &Path, objects: &[PathBuf]) -> Result<(), String> {
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
    let status = Command::new("ar")
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

pub fn assemble_object(asm_path: &Path, obj_path: &Path, target: TargetKind) -> Result<(), String> {
    let mut cmd = Command::new("cc");
    configure_cc_for_target(&mut cmd, target)?;
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

pub fn native_toolchain_supports_target(target: TargetKind) -> bool {
    if cfg!(target_os = "macos") {
        return true;
    }
    target == TargetKind::host()
}

pub(crate) fn configure_cc_for_target(cmd: &mut Command, target: TargetKind) -> Result<(), String> {
    if cfg!(target_os = "macos") {
        cmd.arg("-arch").arg(target.as_str());
        return Ok(());
    }

    if native_toolchain_supports_target(target) {
        Ok(())
    } else {
        Err(format!(
            "native toolchain support for target {} is unavailable on host {}",
            target.as_str(),
            TargetKind::host().as_str()
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
