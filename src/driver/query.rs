use std::path::{Path, PathBuf};

use crate::core::diag::{Position, Span};
use crate::core::resolve::def::DefId;
use crate::services::analysis::db::AnalysisDb;
use crate::services::analysis::query::QueryResult;

#[derive(Debug, Clone, Copy)]
pub enum QueryLookupKind {
    Def,
    Type,
    Hover,
    Completions,
    Signature,
    References,
    Rename,
    DocumentSymbols,
    SemanticTokens,
    CodeActions,
}

#[derive(Debug, Clone)]
struct QueryRequest {
    input_path: PathBuf,
    line: usize,
    col: usize,
    span: Span,
    file_id: crate::services::analysis::snapshot::FileId,
}

struct QueryPrinter<'a> {
    req: &'a QueryRequest,
}

impl<'a> QueryPrinter<'a> {
    fn new(req: &'a QueryRequest) -> Self {
        Self { req }
    }

    fn none_at(&self, what: &str) -> usize {
        println!("[NONE] no {what} at {}:{}", self.req.line, self.req.col);
        1
    }

    fn location(&self, path: Option<&Path>, file_id: u64, span: Span) -> String {
        format_location(path, file_id, span)
    }

    fn input_location(&self, span: Span) -> String {
        self.location(
            Some(self.req.input_path.as_path()),
            self.req.file_id.0 as u64,
            span,
        )
    }
}

/// Execute a single query at a source location and print user-facing output.
pub fn run_query(
    input_path: &Path,
    pos: &str,
    kind: QueryLookupKind,
    new_name: Option<&str>,
) -> Result<usize, String> {
    let mut db = AnalysisDb::new();
    let request = build_query_request(&mut db, input_path, pos)?;
    dispatch_query_kind(&mut db, &request, kind, new_name)
}

fn build_query_request(
    db: &mut AnalysisDb,
    input_path: &Path,
    pos: &str,
) -> Result<QueryRequest, String> {
    let source = std::fs::read_to_string(input_path)
        .map_err(|e| format!("failed to read {}: {e}", input_path.display()))?;
    let (line, col) = parse_pos_arg(pos)?;
    let position = position_from_line_col(&source, line, col)?;
    let span = Span {
        start: position,
        end: position,
    };
    let file_id = db.upsert_disk_text(input_path.to_path_buf(), source.as_str());

    Ok(QueryRequest {
        input_path: input_path.to_path_buf(),
        line,
        col,
        span,
        file_id,
    })
}

fn dispatch_query_kind(
    db: &mut AnalysisDb,
    req: &QueryRequest,
    kind: QueryLookupKind,
    new_name: Option<&str>,
) -> Result<usize, String> {
    match kind {
        QueryLookupKind::Def => run_def_query(db, req),
        QueryLookupKind::Type => run_type_query(db, req),
        QueryLookupKind::Hover => run_hover_query(db, req),
        QueryLookupKind::Completions => run_completions_query(db, req),
        QueryLookupKind::Signature => run_signature_query(db, req),
        QueryLookupKind::References => run_references_query(db, req),
        QueryLookupKind::Rename => run_rename_query(db, req, new_name),
        QueryLookupKind::DocumentSymbols => run_document_symbols_query(db, req),
        QueryLookupKind::SemanticTokens => run_semantic_tokens_query(db, req),
        QueryLookupKind::CodeActions => run_code_actions_query(db, req),
    }
}

fn run_def_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    run_optional_query(
        req,
        "definition",
        || def_at_query_point(db, req),
        |def_id| {
            println!("def {}", def_id.0);
            Ok(0)
        },
    )
}

fn run_type_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    run_optional_query(
        req,
        "type",
        || map_query_cancelled(db.type_at_file(req.file_id, req.span)),
        |ty| {
            println!("{ty}");
            Ok(0)
        },
    )
}

fn run_hover_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    run_optional_query(
        req,
        "hover info",
        || map_query_cancelled(db.hover_at_file(req.file_id, req.span)),
        |hover| {
            println!("{}", hover.display);
            Ok(0)
        },
    )
}

fn run_completions_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let items = map_query_cancelled(db.completions_at_file(req.file_id, req.span))?;
    if items.is_empty() {
        return Ok(printer.none_at("completions"));
    }
    for item in items {
        if let Some(detail) = item.detail {
            println!("{} ({detail})", item.label);
        } else {
            println!("{}", item.label);
        }
    }
    Ok(0)
}

fn run_signature_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    run_optional_query(
        req,
        "signature info",
        || map_query_cancelled(db.signature_help_at_file(req.file_id, req.span)),
        |sig| {
            println!("{}", sig.label);
            println!("active_parameter={}", sig.active_parameter);
            Ok(0)
        },
    )
}

fn run_references_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let def_id = def_at_query_point(db, req)?;
    let Some(def_id) = def_id else {
        return Ok(printer.none_at("definition"));
    };

    let refs = map_query_cancelled(db.references(def_id))?;
    if refs.is_empty() {
        println!("[NONE] no references for def {}", def_id.0);
        return Ok(1);
    }
    println!("def {} references {}", def_id.0, refs.len());
    for loc in refs {
        println!(
            "{}",
            printer.location(loc.path.as_deref(), loc.file_id.0 as u64, loc.span)
        );
    }
    Ok(0)
}

fn run_rename_query(
    db: &mut AnalysisDb,
    req: &QueryRequest,
    new_name: Option<&str>,
) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let Some(new_name) = new_name else {
        return Err("`--new-name` is required for `--kind rename`".to_string());
    };
    let def_id = def_at_query_point(db, req)?;
    let Some(def_id) = def_id else {
        return Ok(printer.none_at("definition"));
    };

    let plan = map_query_cancelled(db.rename_plan(def_id, new_name))?;
    let old_name = plan.old_name.as_deref().unwrap_or("<unknown>");
    let can_apply = plan.can_apply();
    println!(
        "rename {} {} -> {} (edits: {}, conflicts: {}, can_apply: {})",
        plan.def_id.0,
        old_name,
        plan.new_name,
        plan.edits.len(),
        plan.conflicts.len(),
        can_apply
    );
    for conflict in &plan.conflicts {
        match conflict.existing_def {
            Some(existing) => println!("conflict [{}]: {}", existing.0, conflict.message),
            None => println!("conflict: {}", conflict.message),
        }
    }
    for edit in &plan.edits {
        println!(
            "edit {} => `{}`",
            printer.location(
                edit.location.path.as_deref(),
                edit.location.file_id.0 as u64,
                edit.location.span
            ),
            edit.replacement
        );
    }
    if can_apply { Ok(0) } else { Ok(1) }
}

fn run_document_symbols_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let symbols = map_query_cancelled(db.document_symbols_at_file(req.file_id))?;
    if symbols.is_empty() {
        println!("[NONE] no document symbols");
        return Ok(1);
    }
    for sym in symbols {
        println!(
            "{} [{}] {}",
            sym.name,
            sym.def_id.0,
            printer.input_location(sym.span)
        );
    }
    Ok(0)
}

fn run_semantic_tokens_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let tokens = map_query_cancelled(db.semantic_tokens_at_file(req.file_id))?;
    if tokens.is_empty() {
        println!("[NONE] no semantic tokens");
        return Ok(1);
    }
    for token in tokens {
        println!(
            "{:?} [{}] {}",
            token.kind,
            token.def_id.0,
            printer.input_location(token.span)
        );
    }
    Ok(0)
}

fn run_code_actions_query(db: &mut AnalysisDb, req: &QueryRequest) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let actions = map_query_cancelled(db.code_actions_at_file(req.file_id, req.span))?;
    if actions.is_empty() {
        return Ok(printer.none_at("code actions"));
    }
    for action in actions {
        println!("{} [{}]", action.title, action.diagnostic_code);
        for edit in action.edits {
            println!(
                "  edit {} => `{}`",
                printer.input_location(edit.span),
                edit.new_text
            );
        }
    }
    Ok(0)
}

fn format_location(path: Option<&Path>, file_id: u64, span: Span) -> String {
    let base = path
        .map(|p| p.display().to_string())
        .unwrap_or_else(|| format!("<file:{file_id}>"));
    format!(
        "{}:{}:{}-{}:{}",
        base, span.start.line, span.start.column, span.end.line, span.end.column
    )
}

fn def_at_query_point(db: &mut AnalysisDb, req: &QueryRequest) -> Result<Option<DefId>, String> {
    map_query_cancelled(db.def_at_file(req.file_id, req.span))
}

fn run_optional_query<T>(
    req: &QueryRequest,
    none_message: &str,
    query: impl FnOnce() -> Result<Option<T>, String>,
    on_some: impl FnOnce(T) -> Result<usize, String>,
) -> Result<usize, String> {
    let printer = QueryPrinter::new(req);
    let value = query()?;
    match value {
        Some(value) => on_some(value),
        None => Ok(printer.none_at(none_message)),
    }
}

fn map_query_cancelled<T>(result: QueryResult<T>) -> Result<T, String> {
    result.map_err(|_| "analysis query cancelled".to_string())
}

fn parse_pos_arg(pos: &str) -> Result<(usize, usize), String> {
    let (line, col) = pos
        .split_once(':')
        .ok_or_else(|| format!("invalid --pos format `{pos}`; expected line:col"))?;
    let line = line
        .parse::<usize>()
        .map_err(|_| format!("invalid line in --pos `{pos}`"))?;
    let col = col
        .parse::<usize>()
        .map_err(|_| format!("invalid column in --pos `{pos}`"))?;
    if line == 0 || col == 0 {
        return Err(format!("--pos must be 1-based, got `{pos}`"));
    }
    Ok((line, col))
}

fn position_from_line_col(source: &str, line: usize, col: usize) -> Result<Position, String> {
    let mut curr_line = 1usize;
    let mut curr_col = 1usize;
    for (offset, ch) in source.char_indices() {
        if curr_line == line && curr_col == col {
            return Ok(Position {
                offset,
                line,
                column: col,
            });
        }
        if ch == '\n' {
            curr_line += 1;
            curr_col = 1;
        } else {
            curr_col += 1;
        }
    }
    if curr_line == line && curr_col == col {
        return Ok(Position {
            offset: source.len(),
            line,
            column: col,
        });
    }
    Err(format!(
        "position {line}:{col} is outside source range (line={curr_line}, col={curr_col})"
    ))
}
