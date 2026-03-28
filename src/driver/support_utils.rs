use std::ffi::OsStr;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::time::SystemTime;

pub(crate) fn compile_c_object(source: &Path, object: &Path) -> Result<(), String> {
    let status = Command::new("cc")
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
        fs::remove_file(archive)
            .map_err(|e| format!("failed to remove stale {}: {e}", archive.display()))?;
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

pub fn assemble_object(asm_path: &Path, obj_path: &Path) -> Result<(), String> {
    let status = Command::new("cc")
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
