use std::collections::HashMap;
use std::fs;
use std::path::Path;
use std::process::Command;

use serde::Deserialize;

use crate::backend::TargetKind;
use crate::core::capsule::infer_capsule_root;

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
    target_tools: HashMap<TargetKind, TargetToolConfig>,
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
        Self::from_raw(raw)
    }

    pub fn tool(&self, target: TargetKind, tool: ToolKind) -> Option<&ToolCommand> {
        self.target_tools.get(&target).and_then(|cfg| match tool {
            ToolKind::Cc => cfg.cc.as_ref(),
            ToolKind::Ar => cfg.ar.as_ref(),
        })
    }

    pub fn has_tool_override(&self, target: TargetKind) -> bool {
        self.target_tools
            .get(&target)
            .is_some_and(|cfg| cfg.cc.is_some() || cfg.ar.is_some())
    }

    fn from_raw(raw: RawProjectConfig) -> Result<Option<Self>, String> {
        let mut target_tools = HashMap::new();
        for (target_name, raw_target) in raw.target {
            let Some(target) = parse_target_key(&target_name) else {
                return Err(format!("unsupported target key '{target_name}' in machina.toml"));
            };
            let cc = raw_target
                .cc
                .map(|argv| ToolCommand::from_argv(argv, target, ToolKind::Cc))
                .transpose()?;
            let ar = raw_target
                .ar
                .map(|argv| ToolCommand::from_argv(argv, target, ToolKind::Ar))
                .transpose()?;
            target_tools.insert(target, TargetToolConfig { cc, ar });
        }
        if target_tools.is_empty() {
            Ok(None)
        } else {
            Ok(Some(Self { target_tools }))
        }
    }
}

#[derive(Debug, Clone, Default)]
struct TargetToolConfig {
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
    cc: Option<Vec<String>>,
    ar: Option<Vec<String>>,
}

fn parse_target_key(key: &str) -> Option<TargetKind> {
    match key {
        "arm64" => Some(TargetKind::Arm64),
        "x86-64" => Some(TargetKind::X86_64),
        "x86-64-linux" => Some(TargetKind::X86_64Linux),
        _ => None,
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
            .tool(TargetKind::X86_64Linux, ToolKind::Cc)
            .expect("x86_64-linux cc");
        let ar = cfg
            .tool(TargetKind::X86_64Linux, ToolKind::Ar)
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
        assert!(err.contains("unsupported target key 'mips64-linux'"));
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
cc = ["colima", "ssh", "--", "gcc"]
ar = ["colima", "ssh", "--", "ar"]
"#,
        )
        .expect("write machina.toml");

        let cfg = ProjectConfig::load_for_root(&temp_root)
            .expect("load config")
            .expect("present config");
        assert!(cfg.tool(TargetKind::X86_64Linux, ToolKind::Cc).is_some());

        let _ = fs::remove_dir_all(&temp_root);
    }

    fn unique_suffix() -> u128 {
        std::time::SystemTime::now()
            .duration_since(std::time::UNIX_EPOCH)
            .expect("system clock")
            .as_nanos()
    }
}
