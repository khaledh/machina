use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use serde::Deserialize;

use crate::backend::TargetKind;
use crate::core::capsule::infer_capsule_root;
use crate::driver::target::{PlatformKind, parse_builtin_target_name};

#[derive(Debug, Clone)]
pub struct ToolCommand {
    program: String,
    args: Vec<String>,
}

impl ToolCommand {
    fn from_argv(argv: Vec<String>, target: TargetKind, tool: ToolKind) -> Result<Self, String> {
        let mut parts = argv.into_iter();
        let Some(program) = parts.next() else {
            return Err(format!(
                "target {} tool '{}' must have at least one argv element",
                target.config_key(),
                tool.config_key()
            ));
        };
        Ok(Self {
            program,
            args: parts.collect(),
        })
    }

    pub fn to_command(&self) -> Command {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd
    }
}

#[derive(Debug, Clone, Default)]
pub struct ProjectConfig {
    root: Option<PathBuf>,
    targets: HashMap<String, TargetConfig>,
}

impl ProjectConfig {
    pub fn load_for_entry(entry_file: &Path) -> Result<Option<Self>, String> {
        let root = infer_capsule_root(entry_file);
        Self::load_for_root(&root)
    }

    pub fn load_for_root(root: &Path) -> Result<Option<Self>, String> {
        let path = root.join("machina.toml");
        if !path.exists() {
            return Ok(None);
        }
        let text = fs::read_to_string(&path)
            .map_err(|e| format!("failed to read {}: {e}", path.display()))?;
        let raw: RawProjectConfig = toml::from_str(&text)
            .map_err(|e| format!("failed to parse {}: {e}", path.display()))?;
        Self::from_raw_with_root(raw, Some(root))
    }

    pub fn tool(&self, target_name: &str, tool: ToolKind) -> Option<&ToolCommand> {
        self.targets
            .get(&canonical_target_name(target_name))
            .and_then(|cfg| match tool {
                ToolKind::Cc => cfg.cc.as_ref(),
                ToolKind::Ar => cfg.ar.as_ref(),
            })
    }

    pub fn target_kind(&self, target_name: &str) -> Option<TargetKind> {
        self.targets
            .get(&canonical_target_name(target_name))
            .map(|cfg| cfg.kind)
    }

    pub fn target_identity(&self, target_name: &str) -> Option<(TargetKind, PlatformKind)> {
        self.targets
            .get(&canonical_target_name(target_name))
            .map(|cfg| (cfg.kind, cfg.platform))
    }

    pub fn has_tool_override(&self, target_name: &str) -> bool {
        self.targets
            .get(&canonical_target_name(target_name))
            .is_some_and(|cfg| cfg.cc.is_some() || cfg.ar.is_some())
    }

    pub fn linker_script(&self, target_name: &str) -> Option<PathBuf> {
        let cfg = self.targets.get(&canonical_target_name(target_name))?;
        let script = cfg.linker_script.as_ref()?;
        let root = self.root.as_ref()?;
        if script.is_absolute() {
            Some(script.clone())
        } else {
            Some(root.join(script))
        }
    }

    #[cfg(test)]
    fn from_raw(raw: RawProjectConfig) -> Result<Option<Self>, String> {
        Self::from_raw_with_root(raw, None)
    }

    fn from_raw_with_root(
        raw: RawProjectConfig,
        root: Option<&Path>,
    ) -> Result<Option<Self>, String> {
        let mut targets = HashMap::new();
        for (target_name, raw_target) in raw.target {
            let canonical_name = canonical_target_name(&target_name);
            let (target, platform) = resolve_target_kind(&target_name, &raw_target)?;
            let cc = raw_target
                .cc
                .map(|argv| ToolCommand::from_argv(argv, target, ToolKind::Cc))
                .transpose()?;
            let ar = raw_target
                .ar
                .map(|argv| ToolCommand::from_argv(argv, target, ToolKind::Ar))
                .transpose()?;
            targets.insert(
                canonical_name,
                TargetConfig {
                    kind: target,
                    platform,
                    linker_script: raw_target.linker_script.map(PathBuf::from),
                    cc,
                    ar,
                },
            );
        }
        if targets.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Self {
                root: root.map(Path::to_path_buf),
                targets,
            }))
        }
    }
}

#[derive(Debug, Clone)]
struct TargetConfig {
    kind: TargetKind,
    platform: PlatformKind,
    linker_script: Option<PathBuf>,
    cc: Option<ToolCommand>,
    ar: Option<ToolCommand>,
}

#[derive(Debug, Clone, Copy)]
pub enum ToolKind {
    Cc,
    Ar,
}

impl ToolKind {
    fn config_key(self) -> &'static str {
        match self {
            Self::Cc => "cc",
            Self::Ar => "ar",
        }
    }
}

#[derive(Debug, Deserialize, Default)]
struct RawProjectConfig {
    #[serde(default)]
    target: HashMap<String, RawTargetToolConfig>,
}

#[derive(Debug, Deserialize, Default)]
struct RawTargetToolConfig {
    arch: Option<String>,
    platform: Option<String>,
    #[serde(rename = "linker-script")]
    linker_script: Option<String>,
    cc: Option<Vec<String>>,
    ar: Option<Vec<String>>,
}

fn canonical_target_name(name: &str) -> String {
    parse_builtin_target_name(name)
        .map(|kind| kind.config_key().to_string())
        .unwrap_or_else(|| name.to_string())
}

fn resolve_target_kind(
    target_name: &str,
    raw_target: &RawTargetToolConfig,
) -> Result<(TargetKind, PlatformKind), String> {
    if let Some(kind) = parse_builtin_target_name(target_name) {
        if raw_target.arch.is_none() && raw_target.platform.is_none() {
            return Ok((kind, builtin_platform(kind)));
        }

        let declared = target_kind_from_parts(
            raw_target.arch.as_deref(),
            raw_target.platform.as_deref(),
            target_name,
        )?;
        if declared.0 != kind {
            return Err(format!(
                "target `{target_name}` declares arch/platform for {}, but the built-in target name resolves to {}",
                declared.0.config_key(),
                kind.config_key()
            ));
        }
        return Ok(declared);
    }

    target_kind_from_parts(
        raw_target.arch.as_deref(),
        raw_target.platform.as_deref(),
        target_name,
    )
}

fn target_kind_from_parts(
    arch: Option<&str>,
    platform: Option<&str>,
    target_name: &str,
) -> Result<(TargetKind, PlatformKind), String> {
    let Some(arch) = arch else {
        return Err(format!(
            "custom target `{target_name}` must declare `arch = \"...\"` in machina.toml"
        ));
    };
    let Some(platform) = platform else {
        return Err(format!(
            "custom target `{target_name}` must declare `platform = \"...\"` in machina.toml"
        ));
    };

    match (arch, platform) {
        ("arm64", "macos") => Ok((TargetKind::Arm64Macos, PlatformKind::Macos)),
        ("x86-64", "macos") => Ok((TargetKind::X86_64Macos, PlatformKind::Macos)),
        ("x86-64", "linux") => Ok((TargetKind::X86_64Linux, PlatformKind::Linux)),
        ("x86-64", "none") => Ok((TargetKind::X86_64Linux, PlatformKind::None)),
        _ => Err(format!(
            "unsupported target combination for `{target_name}`: arch = `{arch}`, platform = `{platform}`"
        )),
    }
}

fn builtin_platform(kind: TargetKind) -> PlatformKind {
    match kind {
        TargetKind::Arm64Macos | TargetKind::X86_64Macos => PlatformKind::Macos,
        TargetKind::X86_64Linux => PlatformKind::Linux,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn loads_target_toolchains_from_machina_toml() {
        let raw = RawProjectConfig {
            target: HashMap::from([(
                String::from("x86-64-linux"),
                RawTargetToolConfig {
                    arch: None,
                    platform: None,
                    linker_script: None,
                    cc: Some(vec![
                        String::from("colima"),
                        String::from("ssh"),
                        String::from("--"),
                        String::from("gcc"),
                    ]),
                    ar: Some(vec![
                        String::from("colima"),
                        String::from("ssh"),
                        String::from("--"),
                        String::from("ar"),
                    ]),
                },
            )]),
        };

        let cfg = ProjectConfig::from_raw(raw)
            .expect("parse raw config")
            .expect("non-empty config");

        let cc = cfg
            .tool("x86-64-linux", ToolKind::Cc)
            .expect("x86_64-linux cc");
        let ar = cfg
            .tool("x86-64-linux", ToolKind::Ar)
            .expect("x86_64-linux ar");

        let cc_cmd = cc.to_command();
        let ar_cmd = ar.to_command();
        assert_eq!(cc_cmd.get_program(), "colima");
        assert_eq!(
            cc_cmd.get_args().collect::<Vec<_>>(),
            vec!["ssh", "--", "gcc"]
        );
        assert_eq!(ar_cmd.get_program(), "colima");
        assert_eq!(
            ar_cmd.get_args().collect::<Vec<_>>(),
            vec!["ssh", "--", "ar"]
        );
    }

    #[test]
    fn rejects_empty_tool_argv() {
        let raw = RawProjectConfig {
            target: HashMap::from([(
                String::from("x86-64-linux"),
                RawTargetToolConfig {
                    arch: None,
                    platform: None,
                    linker_script: None,
                    cc: Some(Vec::new()),
                    ar: None,
                },
            )]),
        };

        let err = ProjectConfig::from_raw(raw).expect_err("empty argv must error");
        assert!(err.contains("must have at least one argv element"));
    }

    #[test]
    fn rejects_unknown_target_keys() {
        let raw = RawProjectConfig {
            target: HashMap::from([(String::from("mips64-linux"), RawTargetToolConfig::default())]),
        };

        let err = ProjectConfig::from_raw(raw).expect_err("unknown target must error");
        assert!(err.contains("must declare `arch ="));
    }

    #[test]
    fn returns_none_for_empty_target_table() {
        let cfg = ProjectConfig::from_raw(RawProjectConfig::default()).expect("parse empty raw");
        assert!(cfg.is_none());
    }

    #[test]
    fn loads_config_from_project_root_file() {
        let temp_root = std::env::temp_dir().join(format!(
            "machina_project_config_{}_{}",
            std::process::id(),
            unique_suffix()
        ));
        fs::create_dir_all(&temp_root).expect("create temp root");
        fs::write(
            temp_root.join("machina.toml"),
            r#"
[target.x86-64-linux]
arch = "x86-64"
platform = "linux"
cc = ["colima", "ssh", "--", "gcc"]
ar = ["colima", "ssh", "--", "ar"]
"#,
        )
        .expect("write machina.toml");

        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("present config");
        assert!(cfg.tool("x86-64-linux", ToolKind::Cc).is_some());

        let _ = fs::remove_dir_all(&temp_root);
    }

    #[test]
    fn custom_targets_require_arch_and_platform() {
        let raw = RawProjectConfig {
            target: HashMap::from([(
                String::from("x86-64-dev"),
                RawTargetToolConfig {
                    arch: Some(String::from("x86-64")),
                    platform: Some(String::from("linux")),
                    linker_script: None,
                    cc: None,
                    ar: None,
                },
            )]),
        };

        let cfg = ProjectConfig::from_raw(raw)
            .expect("parse custom target")
            .expect("non-empty config");

        assert_eq!(cfg.target_kind("x86-64-dev"), Some(TargetKind::X86_64Linux));
        assert_eq!(
            cfg.target_identity("x86-64-dev"),
            Some((TargetKind::X86_64Linux, PlatformKind::Linux))
        );
    }

    #[test]
    fn built_in_target_aliases_normalize_to_canonical_keys() {
        let raw = RawProjectConfig {
            target: HashMap::from([(
                String::from("arm64"),
                RawTargetToolConfig {
                    arch: None,
                    platform: None,
                    linker_script: None,
                    cc: Some(vec![String::from("cc")]),
                    ar: None,
                },
            )]),
        };

        let cfg = ProjectConfig::from_raw(raw)
            .expect("parse built-in alias")
            .expect("non-empty config");

        assert!(cfg.tool("arm64-macos", ToolKind::Cc).is_some());
    }

    #[test]
    fn bare_custom_targets_resolve_to_none_platform() {
        let raw = RawProjectConfig {
            target: HashMap::from([(
                String::from("x86-64-bare"),
                RawTargetToolConfig {
                    arch: Some(String::from("x86-64")),
                    platform: Some(String::from("none")),
                    linker_script: None,
                    cc: None,
                    ar: None,
                },
            )]),
        };

        let cfg = ProjectConfig::from_raw(raw)
            .expect("parse bare target")
            .expect("non-empty config");

        assert_eq!(
            cfg.target_identity("x86-64-bare"),
            Some((TargetKind::X86_64Linux, PlatformKind::None))
        );
    }

    #[test]
    fn resolves_linker_script_relative_to_project_root() {
        let temp_root = std::env::temp_dir().join(format!(
            "machina_project_linker_script_{}_{}",
            std::process::id(),
            unique_suffix()
        ));
        fs::create_dir_all(temp_root.join("kernel")).expect("create temp root");
        fs::write(
            temp_root.join("machina.toml"),
            r#"
[target.x86-64-bare]
arch = "x86-64"
platform = "none"
linker-script = "kernel/link.ld"
"#,
        )
        .expect("write machina.toml");

        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("present config");

        assert_eq!(
            cfg.linker_script("x86-64-bare"),
            Some(temp_root.join("kernel").join("link.ld"))
        );

        let _ = fs::remove_dir_all(&temp_root);
    }

    fn unique_suffix() -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock")
            .as_nanos()
    }
}
