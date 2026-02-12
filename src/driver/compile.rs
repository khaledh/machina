use std::collections::HashMap;
use std::path::PathBuf;

use crate::analysis::batch::{self, BatchQueryError};
use crate::analysis::db::AnalysisDb;
use crate::backend;
use crate::backend::regalloc::arm64::Arm64Target;
use crate::capsule;
use crate::capsule::ModuleId;
use crate::capsule::compose::{flatten_capsule, merge_modules};
use crate::context::{CapsuleParsedContext, ParsedContext};
use crate::diag::CompileError;
use crate::elaborate;
use crate::ir::GlobalData;
use crate::ir::format::format_func_with_comments_and_names;
use crate::lexer::{LexError, Lexer, Token};
use crate::monomorphize;
use crate::normalize;
use crate::nrvo::NrvoAnalyzer;
use crate::parse::Parser;
use crate::resolve::attach_def_owners;
use crate::semck::sem_check;
use crate::tree::NodeIdGen;
use crate::tree::parsed::Module;

#[derive(Debug)]
pub struct CompileOptions {
    pub dump: Option<String>,
    pub emit_ir: bool,
    pub verify_ir: bool,
    pub trace_alloc: bool,
    pub trace_drops: bool,
    pub inject_prelude: bool,
}

pub struct CompileOutput {
    pub asm: String,
    pub ir: Option<String>,
}

/// Run a module-aware capsule check (parse/resolve/typecheck) without backend lowering.
pub fn check_with_path(
    source: &str,
    source_path: &std::path::Path,
    inject_prelude: bool,
) -> Result<(), Vec<CompileError>> {
    let program =
        capsule::discover_and_parse_capsule(source, source_path).map_err(|e| vec![e.into()])?;
    let program_context = CapsuleParsedContext::new(program);

    let flattened = flatten_capsule(&program_context).map_err(|errs| {
        errs.into_iter()
            .map(CompileError::from)
            .collect::<Vec<CompileError>>()
    })?;
    let user_module = flattened.module;
    let top_level_owners = flattened.top_level_owners;

    let mut id_gen = program_context.next_node_id_gen().clone();
    let (module, id_gen_out) = if inject_prelude {
        let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("std")
            .join("prelude_decl.mc");
        let prelude_src = std::fs::read_to_string(&prelude_path)
            .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;
        let (prelude_module, id_gen_after_prelude) = parse_with_id_gen(&prelude_src, id_gen)?;
        (
            merge_modules(&prelude_module, &user_module),
            id_gen_after_prelude,
        )
    } else {
        (user_module, id_gen)
    };
    id_gen = id_gen_out;

    let ast_context = ParsedContext::new(module, id_gen);
    let module_id = program_context.entry();
    let mut analysis_db = AnalysisDb::new();
    let (resolved_result, mut typed_result) = batch::query_parse_resolve_typecheck(
        &mut analysis_db,
        module_id,
        0,
        ast_context,
        top_level_owners.clone(),
    )
    .map_err(map_batch_query_error)?;
    let resolved_context = resolved_result.into_context();

    // Preserve compile-path parity: re-type-check after monomorphization if instantiations exist.
    if !typed_result.generic_insts.is_empty() {
        let monomorphized_context =
            monomorphize::monomorphize(resolved_context, &typed_result.generic_insts)
                .map_err(|e| vec![e.into()])?;
        typed_result = batch::query_typecheck(&mut analysis_db, module_id, 1, {
            let monomorphized_context = attach_def_owners(monomorphized_context, &top_level_owners);
            crate::analysis::results::ResolvedModuleResult::from_context(
                module_id,
                monomorphized_context,
            )
        })
        .map_err(map_batch_query_error)?;
    }

    let _ = typed_result;
    Ok(())
}

pub fn compile(source: &str, opts: &CompileOptions) -> Result<CompileOutput, Vec<CompileError>> {
    compile_with_path(source, None, opts)
}

pub fn compile_with_path(
    source: &str,
    source_path: Option<&std::path::Path>,
    opts: &CompileOptions,
) -> Result<CompileOutput, Vec<CompileError>> {
    let program_context = if let Some(path) = source_path {
        let program =
            capsule::discover_and_parse_capsule(source, path).map_err(|e| vec![e.into()])?;
        Some(CapsuleParsedContext::new(program))
    } else {
        None
    };

    // Parse dump flags from comma-separated list, e.g. --dump ast,ir,liveness
    let mut dump_tokens = false;
    let mut dump_ast = false;
    let mut dump_def_table = false;
    let mut dump_type_map = false;
    let mut dump_nrvo = false;
    let mut dump_ir = false;
    let mut dump_asm = false;

    if let Some(dump) = &opts.dump {
        for item in dump.split(',').map(|s| s.trim().to_lowercase()) {
            match item.as_str() {
                "tokens" => dump_tokens = true,
                "ast" => dump_ast = true,
                "deftab" => dump_def_table = true,
                "typemap" => dump_type_map = true,
                "nrvo" => dump_nrvo = true,
                "ir" => dump_ir = true,
                "asm" => dump_asm = true,
                "" => {}
                _ => {
                    eprintln!("[WARN] unknown dump flag: {item}");
                }
            }
        }
    }

    // --- Lex (optional dump) ---

    if dump_tokens {
        let lexer = Lexer::new(source);
        let tokens = lexer
            .tokenize()
            .collect::<Result<Vec<Token>, LexError>>()
            .map_err(|e| vec![e.into()])?;

        println!("Tokens:");
        println!("--------------------------------");
        for (i, token) in tokens.iter().enumerate() {
            println!("{}: {}", i, token);
        }
        println!("--------------------------------");
    }

    // --- Parse ---

    let (user_module, id_gen, top_level_owners) = if let Some(program) = &program_context {
        let flattened = flatten_capsule(program).map_err(|errs| {
            errs.into_iter()
                .map(CompileError::from)
                .collect::<Vec<CompileError>>()
        })?;
        (
            flattened.module,
            program.next_node_id_gen().clone(),
            flattened.top_level_owners,
        )
    } else {
        let id_gen = NodeIdGen::new();
        let (module, id_gen) = parse_with_id_gen(source, id_gen)?;
        (module, id_gen, HashMap::new())
    };

    let (module, id_gen) = if opts.inject_prelude {
        // load std/prelude_decl.mc
        let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("std")
            .join("prelude_decl.mc");
        let prelude_src = std::fs::read_to_string(&prelude_path)
            .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;

        let (prelude_module, id_gen) = parse_with_id_gen(&prelude_src, id_gen)?;
        (merge_modules(&prelude_module, &user_module), id_gen)
    } else {
        (user_module, id_gen)
    };

    if dump_ast {
        println!("Parsed Tree:");
        println!("--------------------------------");
        println!("{}", module);
        println!("--------------------------------");
    }

    // --- Resolve Defs/Uses (parsed -> resolved) ---

    let ast_context = ParsedContext::new(module, id_gen);
    let module_id = program_context
        .as_ref()
        .map(CapsuleParsedContext::entry)
        .unwrap_or(ModuleId(0));
    let mut analysis_db = AnalysisDb::new();
    let (resolved_result, mut typed_result) = batch::query_parse_resolve_typecheck(
        &mut analysis_db,
        module_id,
        0,
        ast_context,
        top_level_owners.clone(),
    )
    .map_err(map_batch_query_error)?;
    let resolved_context = resolved_result.into_context();

    if dump_def_table {
        println!("Def Map:");
        println!("--------------------------------");
        println!("{}", resolved_context.def_table);
        println!("--------------------------------");
    }

    // --- Type Check (resolved -> type-checked) ---

    let mut type_checked_context = typed_result.clone().into_context();

    // If any generic instantiations were recorded, monomorphize and re-type-check.
    if !type_checked_context.generic_insts.is_empty() {
        let monomorphized_context =
            monomorphize::monomorphize(resolved_context, &type_checked_context.generic_insts)
                .map_err(|e| vec![e.into()])?;
        typed_result = batch::query_typecheck(&mut analysis_db, module_id, 1, {
            let monomorphized_context = attach_def_owners(monomorphized_context, &top_level_owners);
            crate::analysis::results::ResolvedModuleResult::from_context(
                module_id,
                monomorphized_context,
            )
        })
        .map_err(map_batch_query_error)?;
        type_checked_context = typed_result.into_context();
    }

    if dump_type_map {
        println!("Type Map:");
        println!("--------------------------------");
        println!("{}", type_checked_context.type_map);
        println!("--------------------------------");
    }

    // --- Normalize (typed -> normalized) ---

    let normalized_context = normalize::normalize(type_checked_context);

    // --- Semantic Check ---

    let semantic_checked_context = sem_check(normalized_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    // --- Elaborate (normalized -> semantic) ---

    let elaborated_context = elaborate::elaborate(semantic_checked_context);

    // --- NRVO Analysis ---

    let analyzed_context = NrvoAnalyzer::new(elaborated_context).analyze();

    if dump_nrvo {
        println!("NRVO:");
        println!("--------------------------------");
        for def in analyzed_context.def_table.get_nrvo_eligible_defs() {
            println!("{}", def);
        }
        println!("--------------------------------");
    }

    let target = Arm64Target::new();

    // --- Lower to SSA IR ---
    let lowered = backend::lower::lower_module_with_opts(
        &analyzed_context.module,
        &analyzed_context.def_table,
        &analyzed_context.type_map,
        &analyzed_context.lowering_plans,
        &analyzed_context.drop_plans,
        opts.trace_alloc,
        opts.trace_drops,
    )
    .map_err(|e| vec![e.into()])?;

    // --- Optimize SSA ---
    let mut funcs: Vec<_> = lowered.funcs.iter().map(|f| f.func.clone()).collect();
    let skip_opt = std::env::var("MACHINA_DISABLE_SSA_OPT").ok().is_some();
    let reachable = if skip_opt {
        None
    } else {
        let mut pipeline = backend::opt::Pipeline::new();
        pipeline.run(&mut funcs);
        Some(backend::opt::module_dce::reachable_def_ids(&funcs))
    };

    let mut optimized_funcs = Vec::with_capacity(lowered.funcs.len());
    for (func, lowered_func) in funcs.into_iter().zip(lowered.funcs.iter()) {
        let should_keep = reachable
            .as_ref()
            .map(|defs| defs.contains(&func.def_id))
            .unwrap_or(true);
        if should_keep {
            let types = lowered_func.types.clone();
            let globals = lowered_func.globals.clone();
            optimized_funcs.push(backend::lower::LoweredFunction {
                func,
                types,
                globals,
            });
        }
    }

    let mut lowered = backend::lower::LoweredModule {
        funcs: optimized_funcs,
        globals: lowered.globals.clone(),
    };

    if !skip_opt {
        backend::opt::module_dce::prune_globals(&mut lowered);
    }

    if opts.verify_ir {
        backend::verify::verify_module(&lowered).map_err(|e| vec![e.into()])?;
    }

    let formatted_ir = if opts.emit_ir || dump_ir {
        let mut out = String::new();
        out.push_str(&format_ssa_globals(&lowered.globals));
        for (idx, func) in lowered.funcs.iter().enumerate() {
            if idx > 0 {
                out.push('\n');
            }
            out.push_str(&format_func_with_comments_and_names(
                &func.func,
                &func.types,
                &analyzed_context.symbols.def_names,
            ));
        }
        Some(out)
    } else {
        None
    };

    if dump_ir && let Some(ir) = formatted_ir.as_ref() {
        println!("SSA IR:");
        println!("--------------------------------");
        println!("{ir}");
        println!("--------------------------------");
    }

    let ir = if opts.emit_ir { formatted_ir } else { None };

    let asm =
        backend::codegen::emit_module_arm64(&lowered, &analyzed_context.symbols.def_names, &target);

    if dump_asm {
        println!("ASM:");
        println!("--------------------------------");
        println!("{}", asm);
        println!("--------------------------------");
    }

    Ok(CompileOutput { asm, ir })
}

// --- std parsed-tree injection ---

fn parse_with_id_gen(
    source: &str,
    id_gen: NodeIdGen,
) -> Result<(Module, NodeIdGen), Vec<CompileError>> {
    // Lex
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    // Parse with the given id_gen
    let mut parser = Parser::new_with_id_gen(&tokens, id_gen);
    let module = parser.parse().map_err(|e| vec![e.into()])?;
    let id_gen = parser.into_id_gen();

    Ok((module, id_gen))
}

fn map_batch_query_error(err: BatchQueryError) -> Vec<CompileError> {
    match err {
        BatchQueryError::Cancelled => vec![CompileError::QueryCancelled],
        BatchQueryError::Resolve(errs) => errs.into_iter().map(CompileError::from).collect(),
        BatchQueryError::TypeCheck(errs) => errs.into_iter().map(CompileError::from).collect(),
    }
}

#[cfg(test)]
#[path = "../tests/t_compile.rs"]
mod tests;

// --- Formatting ---
fn format_ssa_globals(globals: &[GlobalData]) -> String {
    let mut out = String::new();
    for global in globals {
        if let Some(text) = format_bytes_as_string(&global.bytes) {
            out.push_str(&format!("global _g{} = \"{}\"\n", global.id.0, text));
            continue;
        }
        out.push_str(&format!("global _g{} = bytes [", global.id.0));
        for (idx, byte) in global.bytes.iter().enumerate() {
            if idx > 0 {
                out.push_str(", ");
            }
            out.push_str(&byte.to_string());
        }
        out.push_str("]\n");
    }
    if !globals.is_empty() {
        out.push('\n');
    }
    out
}

fn format_bytes_as_string(bytes: &[u8]) -> Option<String> {
    let text = std::str::from_utf8(bytes).ok()?;
    let mut out = String::with_capacity(text.len());
    for ch in text.chars() {
        match ch {
            '"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ' ' => out.push(' '),
            _ if ch.is_ascii_graphic() => out.push(ch),
            _ => return None,
        }
    }
    Some(out)
}
