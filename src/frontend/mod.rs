//! Program-level frontend pipeline scaffolding.
//!
//! This layer discovers and parses a module graph before module-local resolve
//! and type checking. It keeps module-loading concerns out of existing parser
//! and resolver logic.

pub mod program;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fmt;
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::diag::Span;
use crate::lexer::{LexError, Lexer, Token};
use crate::parse::{ParseError, Parser};
use crate::tree::NodeIdGen;
use crate::tree::parsed::{Module, Require};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ModulePath {
    segments: Vec<String>,
}

impl ModulePath {
    pub fn new(segments: Vec<String>) -> Result<Self, FrontendError> {
        if segments.is_empty() || segments.iter().any(|s| s.is_empty()) {
            return Err(FrontendError::InvalidModulePath(segments.join(".")));
        }
        Ok(Self { segments })
    }

    pub fn from_require(req: &Require) -> Result<Self, FrontendError> {
        Self::new(req.path.clone())
    }

    pub fn from_file(path: &Path, root: &Path) -> Result<Self, FrontendError> {
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
    pub path: ModulePath,
    pub alias: String,
    pub span: Span,
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

#[derive(Debug, Clone)]
pub struct ProgramParsed {
    pub entry: ModuleId,
    pub modules: HashMap<ModuleId, ParsedModule>,
    pub by_path: HashMap<ModulePath, ModuleId>,
    pub edges: HashMap<ModuleId, Vec<ModuleId>>,
    pub next_node_id_gen: NodeIdGen,
}

impl ProgramParsed {
    pub fn entry_module(&self) -> &ParsedModule {
        self.modules
            .get(&self.entry)
            .expect("frontend entry module should exist")
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
pub enum FrontendError {
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
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError>;
}

pub struct FsModuleLoader {
    project_root: PathBuf,
    std_root: PathBuf,
}

impl FsModuleLoader {
    pub fn new(project_root: PathBuf) -> Self {
        let std_root = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("stdlib");
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
    fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
        let file_path = self.module_to_path(path);
        let source = std::fs::read_to_string(&file_path)
            .map_err(|e| FrontendError::Io(file_path.clone(), e))?;
        Ok((file_path, source))
    }
}

pub fn discover_and_parse_program(
    entry_source: &str,
    entry_file: &Path,
) -> Result<ProgramParsed, FrontendError> {
    let project_root = infer_project_root(entry_file);
    let entry_path = ModulePath::from_file(entry_file, &project_root)?;
    let loader = FsModuleLoader::new(project_root);
    discover_and_parse_program_with_loader(entry_source, entry_file, entry_path, &loader)
}

pub fn discover_and_parse_program_with_loader(
    entry_source: &str,
    entry_file: &Path,
    entry_path: ModulePath,
    loader: &impl ModuleLoader,
) -> Result<ProgramParsed, FrontendError> {
    let mut id_gen = NodeIdGen::new();
    let mut next_module_id = 0u32;

    let entry_module = parse_module(entry_source, entry_file, id_gen)?;
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
        let requires = modules
            .get(&module_id)
            .map(|m| m.requires.clone())
            .unwrap_or_default();
        let mut module_edges = Vec::new();

        for req in requires {
            let dep_id = if let Some(existing) = by_path.get(&req.path) {
                *existing
            } else {
                let (dep_file, dep_source) = loader.load(&req.path)?;
                let (dep_module, next_id_gen) = parse_module(&dep_source, &dep_file, id_gen)?;
                id_gen = next_id_gen;
                let dep_requires = collect_requires(&req.path, &dep_module)?;
                let dep_id = ModuleId(next_module_id);
                next_module_id += 1;
                by_path.insert(req.path.clone(), dep_id);
                modules.insert(
                    dep_id,
                    ParsedModule {
                        source: ModuleSource {
                            id: dep_id,
                            path: req.path.clone(),
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

        edges.insert(module_id, module_edges);
    }

    ensure_acyclic(&modules, &edges)?;

    Ok(ProgramParsed {
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
) -> Result<Vec<RequireSpec>, FrontendError> {
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
            return Err(FrontendError::DuplicateRequireAlias {
                module: module_path.clone(),
                alias,
                span: req.span,
            });
        }
        out.push(RequireSpec {
            path,
            alias,
            span: req.span,
        });
    }
    Ok(out)
}

fn parse_module(
    source: &str,
    path: &Path,
    id_gen: NodeIdGen,
) -> Result<(Module, NodeIdGen), FrontendError> {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|error| FrontendError::Lex {
            path: path.to_path_buf(),
            error,
        })?;

    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse().map_err(|error| FrontendError::Parse {
        path: path.to_path_buf(),
        error,
    })?;
    Ok((module, parser.into_id_gen()))
}

fn infer_project_root(entry_file: &Path) -> PathBuf {
    for ancestor in entry_file.ancestors() {
        if ancestor.join("Cargo.toml").exists() {
            return ancestor.to_path_buf();
        }
    }
    entry_file
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf()
}

fn ensure_acyclic(
    modules: &HashMap<ModuleId, ParsedModule>,
    edges: &HashMap<ModuleId, Vec<ModuleId>>,
) -> Result<(), FrontendError> {
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
    ) -> Result<(), FrontendError> {
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
                return Err(FrontendError::ModuleDependencyCycle(cycle));
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

    struct MockLoader {
        modules: HashMap<String, String>,
    }

    impl ModuleLoader for MockLoader {
        fn load(&self, path: &ModulePath) -> Result<(PathBuf, String), FrontendError> {
            let key = path.to_string();
            if let Some(src) = self.modules.get(&key) {
                Ok((PathBuf::from(format!("{key}.mc")), src.clone()))
            } else {
                Err(FrontendError::UnknownModule(path.clone()))
            }
        }
    }

    #[test]
    fn discover_program_collects_dependencies() {
        let entry_src = r#"
            requires {
                app.util
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert("app.util".to_string(), "fn util() -> u64 { 1 }".to_string());
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let program = discover_and_parse_program_with_loader(
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
    fn discover_program_reports_cycle() {
        let entry_src = r#"
            requires {
                app.a
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.a".to_string(),
            r#"
            requires {
                app.main
            }
            fn a() -> u64 { 1 }
        "#
            .to_string(),
        );
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let err = discover_and_parse_program_with_loader(
            entry_src,
            Path::new("app/main.mc"),
            entry_path,
            &loader,
        )
        .expect_err("cycle should be rejected");

        assert!(matches!(err, FrontendError::ModuleDependencyCycle(_)));
    }

    #[test]
    fn dependency_order_lists_deps_before_entry() {
        let entry_src = r#"
            requires {
                app.a
                app.b
            }
            fn main() -> u64 { 0 }
        "#;
        let mut modules = HashMap::new();
        modules.insert(
            "app.a".to_string(),
            r#"
            requires {
                app.c
            }
            fn a() -> u64 { 1 }
        "#
            .to_string(),
        );
        modules.insert("app.b".to_string(), "fn b() -> u64 { 2 }".to_string());
        modules.insert("app.c".to_string(), "fn c() -> u64 { 3 }".to_string());
        let loader = MockLoader { modules };
        let entry_path = ModulePath::new(vec!["app".to_string(), "main".to_string()]).unwrap();

        let program = discover_and_parse_program_with_loader(
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
                .get(&ModulePath::new(m.split('.').map(|s| s.to_string()).collect()).unwrap())
                .expect("module id should exist");
            order
                .iter()
                .position(|found| *found == id)
                .expect("module should be in order")
        };

        assert!(pos("app.c") < pos("app.a"));
        assert!(pos("app.a") < pos("app.main"));
        assert!(pos("app.b") < pos("app.main"));
    }
}
