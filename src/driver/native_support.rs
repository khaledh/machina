//! Native support facade for batch build/run paths.
//!
//! The implementation is split across focused driver modules:
//! - `runtime_support`: runtime archive caching
//! - `stdlib_support`: cached stdlib object/archive support
//! - `support_utils`: shared artifact helpers

use crate::driver::project_config::ProjectConfig;
use crate::driver::support_utils::cc_command_for_target;
use crate::driver::target::SelectedTarget;
use std::path::{Path, PathBuf};

pub use crate::driver::runtime_support::{
    ensure_runtime_archive, ensure_runtime_core_archive, runtime_source_paths,
};
pub use crate::driver::stdlib_support::{StdlibArtifacts, ensure_stdlib_archive_for_modules};
pub use crate::driver::support_utils::{
    assemble_object, default_exe_path, native_toolchain_supports_target, temp_named_asm_path,
    temp_obj_path,
};

const BARE_RUNTIME_CORE_SYMBOLS: &[&str] = &[
    "__rt_fmt_init",
    "__rt_fmt_append_bytes",
    "__rt_fmt_append_u64",
    "__rt_fmt_append_i64",
    "__rt_fmt_append_bool",
    "__rt_fmt_finish",
    "__rt_u64_to_dec",
    "__rt_i64_to_dec",
    "__rt_u64_to_bin",
    "__rt_u64_to_oct",
    "__rt_u64_to_hex",
    "__rt_string_from_bytes",
];

pub fn link_executable(
    asm_path: &Path,
    extra_objs: &[PathBuf],
    exe_path: &Path,
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<(), String> {
    let mut cmd = build_link_command(asm_path, extra_objs, exe_path, target, project_config)?;
    let status = cmd
        .status()
        .map_err(|e| format!("failed to invoke cc: {e}"))?;
    if status.success() {
        Ok(())
    } else {
        Err(format!("cc exited with status {}", status))
    }
}

fn build_link_command(
    asm_path: &Path,
    extra_objs: &[PathBuf],
    exe_path: &Path,
    target: &SelectedTarget,
    project_config: Option<&ProjectConfig>,
) -> Result<std::process::Command, String> {
    let mut cmd = cc_command_for_target(target, project_config)?;
    cmd.arg("-o").arg(exe_path).arg(asm_path).args(extra_objs);

    if target.platform.is_hosted() {
        let runtime_archive = ensure_runtime_archive(target, project_config)?;
        cmd.arg(runtime_archive);
        return Ok(cmd);
    }

    let Some(linker_script) = project_config.and_then(|cfg| cfg.linker_script(target.config_key()))
    else {
        return Err(format!(
            "target {} uses platform = \"none\" but has no `linker-script = \"...\"` configured in machina.toml",
            target.config_key()
        ));
    };

    cmd.arg("-nostdlib");
    cmd.arg("-static");
    cmd.arg("-no-pie");
    cmd.arg("-Wl,--build-id=none");
    cmd.arg(format!("-Wl,-T,{}", linker_script.display()));
    if bare_asm_requires_runtime_core(asm_path)? {
        let runtime_core_archive = ensure_runtime_core_archive(target, project_config)?;
        cmd.arg(runtime_core_archive);
    }
    Ok(cmd)
}

fn bare_asm_requires_runtime_core(asm_path: &Path) -> Result<bool, String> {
    let asm = std::fs::read_to_string(asm_path)
        .map_err(|e| format!("failed to read {}: {e}", asm_path.display()))?;
    Ok(BARE_RUNTIME_CORE_SYMBOLS
        .iter()
        .any(|symbol| asm.contains(symbol)))
}

#[cfg(test)]
mod tests {
    use super::build_link_command;
    use crate::driver::project_config::ProjectConfig;
    use crate::driver::target::resolve_target;
    use std::fs;
    #[test]
    fn bare_link_command_uses_linker_script_and_skips_runtime() {
        let temp_root = std::env::temp_dir().join(format!(
            "machina_native_support_bare_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(temp_root.join("kernel")).expect("create temp root");
        fs::write(
            temp_root.join("machina.toml"),
            r#"
[target.x86-64-bare]
arch = "x86-64"
platform = "none"
linker-script = "kernel/link.ld"
cc = ["cc"]
"#,
        )
        .expect("write config");
        let asm = temp_root.join("main.s");
        fs::write(&asm, ".text\nmain:\n  ret\n").expect("write asm");
        let exe = temp_root.join("main");
        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("config");
        let target = resolve_target(Some("x86-64-bare"), Some(&cfg)).expect("resolve target");

        let cmd = build_link_command(&asm, &[], &exe, &target, Some(&cfg)).expect("link cmd");
        let args = cmd
            .get_args()
            .map(|arg| arg.to_string_lossy().to_string())
            .collect::<Vec<_>>();

        assert!(args.iter().any(|arg| arg == "-nostdlib"));
        assert!(args.iter().any(|arg| arg == "-static"));
        assert!(args.iter().any(|arg| arg == "-no-pie"));
        assert!(args.iter().any(|arg| arg == "-Wl,--build-id=none"));
        assert!(args.iter().any(|arg| arg.starts_with("-Wl,-T,")
            && arg.ends_with(&format!("kernel{}link.ld", std::path::MAIN_SEPARATOR))));
        assert!(
            !args.iter().any(|arg| arg.contains("libmachina_rt.a")),
            "bare links should not pull in the hosted runtime archive"
        );

        let _ = fs::remove_dir_all(&temp_root);
    }

    #[test]
    fn bare_link_requires_linker_script() {
        let temp_root = std::env::temp_dir().join(format!(
            "machina_native_support_missing_linker_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(&temp_root).expect("create temp root");
        fs::write(
            temp_root.join("machina.toml"),
            r#"
[target.x86-64-bare]
arch = "x86-64"
platform = "none"
cc = ["cc"]
"#,
        )
        .expect("write config");
        let asm = temp_root.join("main.s");
        let exe = temp_root.join("main");
        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("config");
        let target = resolve_target(Some("x86-64-bare"), Some(&cfg)).expect("resolve target");

        let err = build_link_command(&asm, &[], &exe, &target, Some(&cfg))
            .expect_err("missing linker script must error");
        assert!(err.contains("linker-script"));

        let _ = fs::remove_dir_all(&temp_root);
    }

    #[test]
    fn bare_link_command_adds_runtime_core_for_formatting_symbols() {
        let temp_root = std::env::temp_dir().join(format!(
            "machina_native_support_bare_runtime_core_{}_{}",
            std::process::id(),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .expect("clock")
                .as_nanos()
        ));
        fs::create_dir_all(temp_root.join("kernel")).expect("create temp root");
        fs::write(
            temp_root.join("machina.toml"),
            r#"
[target.x86-64-bare]
arch = "x86-64"
platform = "none"
linker-script = "kernel/link.ld"
cc = ["cc"]
ar = ["ar"]
"#,
        )
        .expect("write config");
        let asm = temp_root.join("main.s");
        fs::write(&asm, "  call __rt_fmt_init\n").expect("write asm");
        let exe = temp_root.join("main");
        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("config");
        let target = resolve_target(Some("x86-64-bare"), Some(&cfg)).expect("resolve target");

        let cmd = build_link_command(&asm, &[], &exe, &target, Some(&cfg)).expect("link cmd");
        let args = cmd
            .get_args()
            .map(|arg| arg.to_string_lossy().to_string())
            .collect::<Vec<_>>();

        assert!(
            args.iter().any(|arg| arg.contains("libmachina_rt_core.a")),
            "expected bare formatting link to pull in runtime core archive: {args:?}"
        );

        let _ = fs::remove_dir_all(&temp_root);
    }
}
