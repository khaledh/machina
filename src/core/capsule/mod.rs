//! Capsule-level frontend pipeline scaffolding.
//!
//! This layer discovers and parses a module graph before module-local resolve
//! and type checking. It keeps module-loading concerns out of existing parser
//! and resolver logic.

pub mod bind;
pub mod compose;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::core::diag::Span;
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::parse::{ParseError, Parser, ParserOptions};
use crate::core::tree::{Module, NodeIdGen, Require};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    segments: Vec<String>,
}

impl ModulePath {
    pub fn new(segments: Vec<String>) -> Result<Self, CapsuleError> {
        if segments.is_empty() || segments.iter().any(|s| s.is_empty()) {
            return Err(CapsuleError::InvalidModulePath(segments.join(".")));
        }
        Ok(Self { segments })
    }

    pub fn from_require(req: &Require) -> Result<Self, CapsuleError> {
        Self::new(req.path.clone())
    }

    pub fn from_file(path: &Path, root: &Path) -> Result<Self, CapsuleError> {
        let rel = path.strip_prefix(root).unwrap_or(path);
        let mut segments = rel
            .iter()
            .map(|s| s.to_string_lossy().to_string())
            .collect::<Vec<_>>();
        if let Some(last) = segments.last_mut()
            && last.ends_with(".mc")
        {
            *last = last.trim_end_matches(".mc").to_string();
        }
        Self::new(segments)
    }

    pub fn segments(&self) -> &[String] {
        &self.segments
    }
}

impl fmt::Display for ModulePath {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.segments.join("."))
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

#[derive(Debug, Clone)]
pub struct RequireSpec {
    pub module_path: ModulePath,
    pub member: Option<String>,
    pub kind: RequireKind,
    pub alias: String,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RequireKind {
    Module,
    Symbol,
}

#[derive(Debug, Clone)]
pub struct ModuleSource {
    pub id: ModuleId,
    pub path: ModulePath,
    pub file_path: PathBuf,
    pub source: String,
}

#[derive(Debug, Clone)]
pub struct ParsedModule {
    pub source: ModuleSource,
    pub module: Module,
    pub requires: Vec<RequireSpec>,
}

#[derive(Debug, Clone, Copy, Default)]
pub struct CapsuleParseOptions {
    pub experimental_typestate: bool,
}

#[derive(Debug, Clone)]
pub struct CapsuleParsed {
    pub entry: ModuleId,
    pub modules: HashMap<ModuleId, ParsedModule>,
    pub by_path: HashMap<ModulePath, ModuleId>,
    pub edges: HashMap<ModuleId, Vec<ModuleId>>,
    pub next_node_id_gen: NodeIdGen,
}

impl CapsuleParsed {
    pub fn entry_module(&self) -> &ParsedModule {
        self.modules
            .get(&self.entry)
            .expect("capsule entry module should exist")
    }

    pub fn module(&self, id: ModuleId) -> Option<&ParsedModule> {
        self.modules.get(&id)
    }

    /// Returns modules reachable from entry in dependency-first order.
    pub fn dependency_order_from_entry(&self) -> Vec<ModuleId> {
        fn visit(
            id: ModuleId,
            edges: &HashMap<ModuleId, Vec<ModuleId>>,
            seen: &mut HashSet<ModuleId>,
            out: &mut Vec<ModuleId>,
        ) {
            if !seen.insert(id) {
                return;
            }
            if let Some(deps) = edges.get(&id) {
                for dep in deps {
                    visit(*dep, edges, seen, out);
                }
            }
            out.push(id);
        }

        let mut seen = HashSet::new();
        let mut out = Vec::new();
        visit(self.entry, &self.edges, &mut seen, &mut out);
        out
    }
}

#[derive(Debug, Error)]
pub enum CapsuleError {
    #[error("invalid module path: {0}")]
    InvalidModulePath(String),

    #[error("unknown module: {0}")]
    UnknownModule(ModulePath),

    #[error("duplicate requires alias `{alias}` in module `{module}`")]
    DuplicateRequireAlias {
        module: ModulePath,
        alias: String,
        span: Span,
    },

    #[error("unknown requires alias `{alias}`")]
    UnknownRequireAlias { alias: String, span: Span },

    #[error("module `{module}` imported as `{alias}` does not export {expected_kind} `{member}`")]
    RequireMemberUndefined {
        alias: String,
        module: ModulePath,
        member: String,
        expected_kind: &'static str,
        span: Span,
    },

    #[error("{expected_kind} `{member}` in module `{module}` (imported as `{alias}`) is private")]
    RequireMemberPrivate {
        alias: String,
        module: ModulePath,
        member: String,
        expected_kind: &'static str,
        span: Span,
    },

    #[error("module dependency cycle detected: {0}")]
    ModuleDependencyCycle(String),

    #[error("symbol import aliasing is not supported yet: `{module}::{member} as {alias}`")]
    SymbolImportAliasUnsupported {
        module: ModulePath,
        member: String,
        alias: String,
        span: Span,
    },

    #[error("failed to read module file {0}: {1}")]
    Io(PathBuf, std::io::Error),

    #[error("lex error in {path}: {error}")]
    Lex {
        path: PathBuf,
        #[source]
        error: LexError,
    },

    #[error("parse error in {path}: {error}")]
    Parse {
        path: PathBuf,
        #[source]
        error: ParseError,
    },
}

pub trait ModuleLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), CapsuleError>;
}

pub struct FsModuleLoader {
    project_root: PathBuf,
    std_root: PathBuf,
}

impl FsModuleLoader {
    pub fn new(project_root: PathBuf) -> Self {
        let std_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("std");
        Self {
            project_root,
            std_root,
        }
    }

    fn module_to_path(&self, path: &ModulePath) -> PathBuf {
        let mut out = if path.segments().first().map(String::as_str) == Some("std") {
            let mut p = self.std_root.clone();
            for seg in path.segments().iter().skip(1) {
                p.push(seg);
            }
            p
        } else {
            let mut p = self.project_root.clone();
            for seg in path.segments() {
                p.push(seg);
            }
            p
        };
        out.set_extension("mc");
        out
    }
}

impl ModuleLoader for FsModuleLoader {
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), CapsuleError> {
        let file_path = self.module_to_path(path);
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| CapsuleError::Io(file_path.clone(), e))?;
        Ok((file_path, source))
    }
}

pub fn discover_and_parse_capsule(
    entry_source: &str,
    entry_file: &Path,
) -> Result<CapsuleParsed, CapsuleError> {
    discover_and_parse_capsule_with_options(
        entry_source,
        entry_file,
        CapsuleParseOptions::default(),
    )
}

pub fn discover_and_parse_capsule_with_options(
    entry_source: &str,
    entry_file: &Path,
    options: CapsuleParseOptions,
) -> Result<CapsuleParsed, CapsuleError> {
    let project_root = infer_project_root(entry_file);
    let entry_path = ModulePath::from_file(entry_file, &project_root)?;
    let loader = FsModuleLoader::new(project_root);
    discover_and_parse_capsule_with_loader_and_options(
        entry_source,
        entry_file,
        entry_path,
        &loader,
        options,
    )
}

pub fn discover_and_parse_capsule_with_loader(
    entry_source: &str,
    entry_file: &Path,
    entry_path: ModulePath,
    loader: &impl ModuleLoader,
) -> Result<CapsuleParsed, CapsuleError> {
    discover_and_parse_capsule_with_loader_and_options(
        entry_source,
        entry_file,
        entry_path,
        loader,
        CapsuleParseOptions::default(),
    )
}

pub fn discover_and_parse_capsule_with_loader_and_options(
    entry_source: &str,
    entry_file: &Path,
    entry_path: ModulePath,
    loader: &impl ModuleLoader,
    options: CapsuleParseOptions,
) -> Result<CapsuleParsed, CapsuleError> {
    let mut id_gen = NodeIdGen::new();
    let mut next_module_id = 0u32;

    let entry_module = parse_module(entry_source, entry_file, id_gen, options)?;
    id_gen = entry_module.1;
    let entry_requires = collect_requires(&entry_path, &entry_module.0)?;

    let entry_id = ModuleId(next_module_id);
    next_module_id += 1;
    let entry_source_data = ModuleSource {
        id: entry_id,
        path: entry_path.clone(),
        file_path: entry_file.to_path_buf(),
        source: entry_source.to_string(),
    };

    let mut modules = HashMap::new();
    modules.insert(
        entry_id,
        ParsedModule {
            source: entry_source_data,
            module: entry_module.0,
            requires: entry_requires,
        },
    );

    let mut by_path = HashMap::new();
    by_path.insert(entry_path.clone(), entry_id);

    let mut edges: HashMap<ModuleId, Vec<ModuleId>> = HashMap::new();
    let mut queue = VecDeque::new();
    queue.push_back(entry_id);

    while let Some(module_id) = queue.pop_front() {
        let mut requires = modules
            .get(&module_id)
            .map(|m| m.requires.clone())
            .unwrap_or_default();
        let mut module_edges = Vec::new();

        for req in &mut requires {
            resolve_require_target(req, &by_path, loader)?;

            let dep_id = if let Some(existing) = by_path.get(&req.module_path) {
                *existing
            } else {
                let (dep_file, dep_source) = loader.load(&req.module_path)?;
                let (dep_module, next_id_gen) =
                    parse_module(&dep_source, &dep_file, id_gen, options)?;
                id_gen = next_id_gen;
                let dep_requires = collect_requires(&req.module_path, &dep_module)?;
                let dep_id = ModuleId(next_module_id);
                next_module_id += 1;
                by_path.insert(req.module_path.clone(), dep_id);
                modules.insert(
                    dep_id,
                    ParsedModule {
                        source: ModuleSource {
                            id: dep_id,
                            path: req.module_path.clone(),
                            file_path: dep_file,
                            source: dep_source,
                        },
                        module: dep_module,
                        requires: dep_requires,
                    },
                );
                queue.push_back(dep_id);
                dep_id
            };
            module_edges.push(dep_id);
        }

        if let Some(module) = modules.get_mut(&module_id) {
            module.requires = requires;
        }
        edges.insert(module_id, module_edges);
    }

    ensure_acyclic(&modules, &edges)?;

    Ok(CapsuleParsed {
        entry: entry_id,
        modules,
        by_path,
        edges,
        next_node_id_gen: id_gen,
    })
}

fn collect_requires(
    module_path: &ModulePath,
    module: &Module,
) -> Result<Vec<RequireSpec>, CapsuleError> {
    let mut seen = HashSet::new();
    let mut out = Vec::new();
    for req in &module.requires {
        let path = ModulePath::from_require(req)?;
        let alias = req
            .alias
            .clone()
            .or_else(|| req.path.last().cloned())
            .unwrap_or_default();
        if !seen.insert(alias.clone()) {
            return Err(CapsuleError::DuplicateRequireAlias {
                module: module_path.clone(),
                alias,
                span: req.span,
            });
        }
        out.push(RequireSpec {
            module_path: path,
            member: None,
            kind: RequireKind::Module,
            alias,
            span: req.span,
        });
    }
    Ok(out)
}

fn resolve_require_target(
    req: &mut RequireSpec,
    by_path: &HashMap<ModulePath, ModuleId>,
    loader: &impl ModuleLoader,
) -> Result<(), CapsuleError> {
    // Exact module path always wins when it exists.
    if by_path.contains_key(&req.module_path) || loader.load(&req.module_path).is_ok() {
        req.kind = RequireKind::Module;
        req.member = None;
        return Ok(());
    }

    // Fallback: treat `<module>::<symbol>` as importing `symbol` from `<module>`.
    let segments = req.module_path.segments();
    if segments.len() < 2 {
        return Err(CapsuleError::UnknownModule(req.module_path.clone()));
    }

    let parent = ModulePath::new(segments[..segments.len() - 1].to_vec())?;
    let member = segments.last().cloned().unwrap_or_default();
    if by_path.contains_key(&parent) || loader.load(&parent).is_ok() {
        if req.alias != member {
            return Err(CapsuleError::SymbolImportAliasUnsupported {
                module: parent,
                member,
                alias: req.alias.clone(),
                span: req.span,
            });
        }
        req.module_path = parent;
        req.member = Some(req.alias.clone());
        req.kind = RequireKind::Symbol;
        return Ok(());
    }

    Err(CapsuleError::UnknownModule(req.module_path.clone()))
}

fn parse_module(
    source: &str,
    path: &Path,
    id_gen: NodeIdGen,
    options: CapsuleParseOptions,
) -> Result<(Module, NodeIdGen), CapsuleError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|error| CapsuleError::Lex {
            path: path.to_path_buf(),
            error,
        })?;

    let mut parser = Parser::new_with_id_gen_and_options(
        &tokens,
        id_gen,
        ParserOptions {
            experimental_typestate: options.experimental_typestate,
        },
    );
    let module = parser.parse().map_err(|error| CapsuleError::Parse {
        path: path.to_path_buf(),
        error,
    })?;
    Ok((module, parser.into_id_gen()))
}

fn infer_project_root(entry_file: &Path) -> PathBuf {
    if let Some(root) = infer_env_capsule_root(entry_file) {
        return root;
    }
    for ancestor in entry_file.ancestors() {
        if ancestor.join("machina.toml").exists() {
            return ancestor.to_path_buf();
        }
    }
    for ancestor in entry_file.ancestors() {
        if ancestor.join(".git").exists() {
            return ancestor.to_path_buf();
        }
    }
    entry_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

fn infer_env_capsule_root(entry_file: &Path) -> Option<PathBuf> {
    let configured = std::env::var_os("MACHINA_CAPSULE_ROOT")?;
    let configured = PathBuf::from(configured);
    let configured = configured.canonicalize().ok().unwrap_or(configured);
    if !configured.is_dir() {
        return None;
    }
    let entry = entry_file
        .canonicalize()
        .ok()
        .unwrap_or_else(|| entry_file.to_path_buf());
    if entry.starts_with(&configured) {
        Some(configured)
    } else {
        None
    }
}

pub fn infer_capsule_root(entry_file: &Path) -> PathBuf {
    infer_project_root(entry_file)
}

fn ensure_acyclic(
    modules: &HashMap<ModuleId, ParsedModule>,
    edges: &HashMap<ModuleId, Vec<ModuleId>>,
) -> Result<(), CapsuleError> {
    #[derive(Clone, Copy, PartialEq, Eq)]
    enum Mark {
        Visiting,
        Visited,
    }

    fn dfs(
        node: ModuleId,
        modules: &HashMap<ModuleId, ParsedModule>,
        edges: &HashMap<ModuleId, Vec<ModuleId>>,
        marks: &mut HashMap<ModuleId, Mark>,
        stack: &mut Vec<ModuleId>,
    ) -> Result<(), CapsuleError> {
        if let Some(mark) = marks.get(&node) {
            if *mark == Mark::Visiting {
                let cycle = stack
                    .iter()
                    .copied()
                    .skip_while(|id| *id != node)
                    .chain(std::iter::once(node))
                    .filter_map(|id| modules.get(&id).map(|m| m.source.path.to_string()))
                    .collect::<Vec<_>>()
                    .join(" -> ");
                return Err(CapsuleError::ModuleDependencyCycle(cycle));
            }
            return Ok(());
        }

        marks.insert(node, Mark::Visiting);
        stack.push(node);
        if let Some(next) = edges.get(&node) {
            for dep in next {
                dfs(*dep, modules, edges, marks, stack)?;
            }
        }
        stack.pop();
        marks.insert(node, Mark::Visited);
        Ok(())
    }

    let mut marks = HashMap::new();
    let mut stack = Vec::new();
    for node in modules.keys().copied() {
        dfs(node, modules, edges, &mut marks, &mut stack)?;
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use std::sync::atomic::{AtomicU64, Ordering};

    static CAPSULE_TMP_COUNTER: AtomicU64 = AtomicU64::new(1);

    struct MockLoader {
        modules: HashMap<String, String>,
    }

    impl ModuleLoader for MockLoader {
        fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), CapsuleError> {
            let key = path.to_string();
            if let Some(src) = self.modules.get(&key) {
                Ok((PathBuf::from(format!("{key}.mc")), src.clone()))
            } else {
                Err(CapsuleError::UnknownModule(path.clone()))
            }
        }
    }

    #[test]
    fn discover_capsule_collects_dependencies() {
        let entry_src = r#"
            requires {
                app::util
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert("app.util".to_string(), "fn util() -> u64 { 1 }".to_string());
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let program = discover_and_parse_capsule_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect("program should parse");

        assert_eq!(program.modules.len(), 2);
        assert_eq!(program.edges.get(&program.entry).map(|v| v.len()), Some(1));
    }

    #[test]
    fn discover_capsule_reports_cycle() {
        let entry_src = r#"
            requires {
                app::a
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.a".to_string(),
            r#"
            requires {
                app::main
            }
            fn a() -> u64 { 1 }
        "#
            .to_string(),
        );
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let err = discover_and_parse_capsule_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect_err("cycle should be rejected");

        assert!(matches!(err, CapsuleError::ModuleDependencyCycle(_)));
    }

    #[test]
    fn dependency_order_lists_deps_before_entry() {
        let entry_src = r#"
            requires {
                app::a
                app::b
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.a".to_string(),
            r#"
            requires {
                app::c
            }
            fn a() -> u64 { 1 }
        "#
            .to_string(),
        );
        modules.insert("app.b".to_string(), "fn b() -> u64 { 2 }".to_string());
        modules.insert("app.c".to_string(), "fn c() -> u64 { 3 }".to_string());
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let program = discover_and_parse_capsule_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect("program should parse");

        let order = program.dependency_order_from_entry();
        let pos = |m: &str| {
            let id = *program
                .by_path
                .get(&ModulePath::new(m.split("::").map(|s| s.to_string()).collect()).unwrap())
                .expect("module id should exist");
            order
                .iter()
                .position(|found| *found == id)
                .expect("module should be in order")
        };

        assert!(pos("app::c") < pos("app::a"));
        assert!(pos("app::a") < pos("app::main"));
        assert!(pos("app::b") < pos("app::main"));
    }

    #[test]
    fn discover_capsule_resolves_symbol_import_to_parent_module() {
        let entry_src = r#"
            requires {
                app::util::answer
            }
            fn main() -> u64 { answer() }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.util".to_string(),
            "@public fn answer() -> u64 { 7 }".to_string(),
        );
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let program = discover_and_parse_capsule_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect("program should parse");

        let entry = program.entry_module();
        assert_eq!(entry.requires.len(), 1);
        let req = &entry.requires[0];
        assert_eq!(
            req.module_path,
            ModulePath::new(vec!["app".to_string(), "util".to_string()]).unwrap()
        );
        assert_eq!(req.member.as_deref(), Some("answer"));
        assert_eq!(req.kind, RequireKind::Symbol);
        assert_eq!(req.alias, "answer");
    }

    #[test]
    fn discover_capsule_rejects_symbol_import_alias_for_now() {
        let entry_src = r#"
            requires {
                app::util::answer as a
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.util".to_string(),
            "@public fn answer() -> u64 { 7 }".to_string(),
        );
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let err = discover_and_parse_capsule_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect_err("symbol import aliases should be rejected");
        assert!(matches!(
            err,
            CapsuleError::SymbolImportAliasUnsupported { .. }
        ));
    }

    #[test]
    fn infer_capsule_root_uses_git_workspace_without_cargo() {
        let run_id = CAPSULE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let temp_root = std::env::temp_dir().join(format!(
            "machina_capsule_root_git_{}_{}",
            std::process::id(),
            run_id
        ));
        let src_dir = temp_root.join("app").join("src");
        fs::create_dir_all(&src_dir).expect("failed to create temp source tree");
        fs::create_dir_all(temp_root.join(".git")).expect("failed to create fake git dir");
        let entry = src_dir.join("main.mc");
        fs::write(&entry, "fn main() -> u64 { 0 }").expect("failed to write entry file");

        let inferred = infer_capsule_root(&entry);
        assert_eq!(inferred, temp_root);

        let _ = fs::remove_dir_all(&temp_root);
    }

    #[test]
    fn infer_capsule_root_prefers_machina_toml_anchor() {
        let run_id = CAPSULE_TMP_COUNTER.fetch_add(1, Ordering::Relaxed);
        let temp_root = std::env::temp_dir().join(format!(
            "machina_capsule_root_anchor_{}_{}",
            std::process::id(),
            run_id
        ));
        let nested = temp_root.join("workspace").join("pkg").join("src");
        fs::create_dir_all(&nested).expect("failed to create temp source tree");
        fs::write(temp_root.join("workspace").join("machina.toml"), "")
            .expect("failed to create capsule root config");
        let entry = nested.join("main.mc");
        fs::write(&entry, "fn main() -> u64 { 0 }").expect("failed to write entry file");

        let inferred = infer_capsule_root(&entry);
        assert_eq!(inferred, temp_root.join("workspace"));

        let _ = fs::remove_dir_all(&temp_root);
    }
}
