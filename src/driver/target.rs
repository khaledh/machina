use crate::backend::{PlatformKind, TargetKind};
use crate::driver::project_config::ProjectConfig;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SelectedTarget {
    pub kind: TargetKind,
    pub platform: PlatformKind,
    name: String,
}

impl SelectedTarget {
    pub fn builtin(kind: TargetKind) -> Self {
        Self {
            kind,
            platform: builtin_platform(kind),
            name: kind.config_key().to_string(),
        }
    }

    pub fn named(kind: TargetKind, platform: PlatformKind, name: impl Into<String>) -> Self {
        Self {
            kind,
            platform,
            name: name.into(),
        }
    }

    pub fn config_key(&self) -> &str {
        &self.name
    }
}

pub fn resolve_target(
    requested: Option<&str>,
    project_config: Option<&ProjectConfig>,
) -> Result<SelectedTarget, String> {
    let Some(requested) = requested else {
        return Ok(SelectedTarget::builtin(TargetKind::host()));
    };

    if let Some(kind) = parse_builtin_target_name(requested) {
        return Ok(SelectedTarget::builtin(kind));
    }

    let Some((kind, platform)) = project_config.and_then(|cfg| cfg.target_identity(requested))
    else {
        return Err(format!(
            "unknown target '{requested}'; expected a built-in target like `arm64-macos`, `x86-64-macos`, or `x86-64-linux`, or a custom target defined in machina.toml"
        ));
    };

    Ok(SelectedTarget::named(kind, platform, requested))
}

pub fn parse_builtin_target_name(name: &str) -> Option<TargetKind> {
    match name {
        "arm64" | "arm64-macos" => Some(TargetKind::Arm64Macos),
        "x86-64" | "x86-64-macos" => Some(TargetKind::X86_64Macos),
        "x86-64-linux" => Some(TargetKind::X86_64Linux),
        _ => None,
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
    use super::{TargetKind, parse_builtin_target_name};

    #[test]
    fn builtin_target_aliases_resolve_to_canonical_kinds() {
        assert_eq!(
            parse_builtin_target_name("arm64"),
            Some(TargetKind::Arm64Macos)
        );
        assert_eq!(
            parse_builtin_target_name("arm64-macos"),
            Some(TargetKind::Arm64Macos)
        );
        assert_eq!(
            parse_builtin_target_name("x86-64"),
            Some(TargetKind::X86_64Macos)
        );
        assert_eq!(
            parse_builtin_target_name("x86-64-macos"),
            Some(TargetKind::X86_64Macos)
        );
        assert_eq!(
            parse_builtin_target_name("x86-64-linux"),
            Some(TargetKind::X86_64Linux)
        );
    }
}
