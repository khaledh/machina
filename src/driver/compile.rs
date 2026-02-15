use std::collections::HashMap;
use std::path::PathBuf;

use crate::core::api::{
    FrontendPolicy, ParseModuleError, ParseModuleOptions, ResolveInputs, elaborate_stage,
    parse_module_with_id_gen_and_options, semcheck_stage,
};
use crate::core::backend;
use crate::core::backend::regalloc::arm64::Arm64Target;
use crate::core::capsule;
use crate::core::capsule::ModuleId;
use crate::core::capsule::compose::{flatten_capsule, merge_modules};
use crate::core::context::{CapsuleParsedContext, ParsedContext};
use crate::core::diag::CompileError;
use crate::core::ir::format::{format_func_with_comments_and_names, format_globals};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::monomorphize;
use crate::core::nrvo::NrvoAnalyzer;
use crate::core::tree::NodeId;
use crate::core::tree::NodeIdGen;
use crate::core::tree::parsed::Module as ParsedModule;

#[derive(Debug)]
pub struct CompileOptions {
    pub dump: Option<String>,
    pub emit_ir: bool,
    pub verify_ir: bool,
    pub trace_alloc: bool,
    pub trace_drops: bool,
    pub inject_prelude: bool,
    pub experimental_typestate: bool,
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
    experimental_typestate: bool,
) -> Result<(), Vec<CompileError>> {
    let program = capsule::discover_and_parse_capsule_with_options(
        source,
        source_path,
        capsule::CapsuleParseOptions {
            experimental_typestate,
        },
    )
    .map_err(|e| vec![e.into()])?;
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
        let (prelude_module, id_gen_after_prelude) =
            parse_with_id_gen(&prelude_src, id_gen, experimental_typestate)?;
        (
            merge_modules(&prelude_module, &user_module),
            id_gen_after_prelude,
        )
    } else {
        (user_module, id_gen)
    };
    id_gen = id_gen_out;

    let ast_context = ParsedContext::new(module, id_gen);
    let _ = resolve_and_typecheck_strict(ast_context, &top_level_owners)?;
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
        let program = capsule::discover_and_parse_capsule_with_options(
            source,
            path,
            capsule::CapsuleParseOptions {
                experimental_typestate: opts.experimental_typestate,
            },
        )
        .map_err(|e| vec![e.into()])?;
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
        let (module, id_gen) = parse_with_id_gen(source, id_gen, opts.experimental_typestate)?;
        (module, id_gen, HashMap::new())
    };

    let (module, id_gen) = if opts.inject_prelude {
        // load std/prelude_decl.mc
        let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("std")
            .join("prelude_decl.mc");
        let prelude_src = std::fs::read_to_string(&prelude_path)
            .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;

        let (prelude_module, id_gen) =
            parse_with_id_gen(&prelude_src, id_gen, opts.experimental_typestate)?;
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
    let (resolved_context, type_checked_context) =
        resolve_and_typecheck_strict(ast_context, &top_level_owners)?;

    if dump_def_table {
        println!("Def Map:");
        println!("--------------------------------");
        println!("{}", resolved_context.def_table);
        println!("--------------------------------");
    }

    // --- Type Check (resolved -> type-checked) ---

    if dump_type_map {
        println!("Type Map:");
        println!("--------------------------------");
        println!("{}", type_checked_context.type_map);
        println!("--------------------------------");
    }

    // --- Semantic Check ---

    let semantic_checked_context = semcheck_stage(type_checked_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    // --- Elaborate (normalized -> semantic) ---

    let elaborated_context = elaborate_stage(semantic_checked_context);

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
    let lowered = backend::lower::lower_module_with_machine_plans_with_opts(
        &analyzed_context.module,
        &analyzed_context.def_table,
        &analyzed_context.type_map,
        &analyzed_context.lowering_plans,
        &analyzed_context.drop_plans,
        &analyzed_context.machine_plans,
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
        let reachable_keep = reachable
            .as_ref()
            .map(|defs| defs.contains(&func.def_id))
            .unwrap_or(true);
        // Keep managed dispatch thunks alive even when not yet reachable from
        // `main`; runtime bootstrap consumes them via descriptor tables.
        let machine_thunk_keep = func.name.starts_with("__mc_machine_dispatch_thunk_");
        if reachable_keep || machine_thunk_keep {
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
        out.push_str(&format_globals(&lowered.globals));
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
    experimental_typestate: bool,
) -> Result<(ParsedModule, NodeIdGen), Vec<CompileError>> {
    parse_module_with_id_gen_and_options(
        source,
        id_gen,
        ParseModuleOptions {
            experimental_typestate,
        },
    )
    .map_err(|e| match e {
        ParseModuleError::Lex(e) => vec![e.into()],
        ParseModuleError::Parse(e) => vec![e.into()],
    })
}

fn resolve_and_typecheck_strict(
    ast_context: ParsedContext,
    top_level_owners: &HashMap<NodeId, ModuleId>,
) -> Result<
    (
        crate::core::context::ResolvedContext,
        crate::core::context::TypeCheckedContext,
    ),
    Vec<CompileError>,
> {
    // Phase 1: resolve + initial strict typecheck on the flattened module.
    let first_pass = crate::core::api::resolve_typecheck_pipeline_with_policy(
        ast_context,
        ResolveInputs::default(),
        Some(top_level_owners),
        FrontendPolicy::Strict,
    );
    if !first_pass.resolve_errors.is_empty() {
        return Err(first_pass
            .resolve_errors
            .into_iter()
            .map(CompileError::from)
            .collect());
    }
    if !first_pass.type_errors.is_empty() {
        return Err(first_pass
            .type_errors
            .into_iter()
            .map(CompileError::from)
            .collect());
    }

    // Extract strict-pass products (safe because we already checked errors).
    let resolved_context = first_pass
        .resolved_context
        .expect("strict resolve should produce context when no errors");
    let typed_context = first_pass
        .typed_context
        .expect("strict typecheck should produce context when no errors");

    // Phase 2: monomorphize + sparse retype (internally handled by core).
    let (resolved_context, typed_context, _mono_stats) =
        monomorphize::monomorphize(resolved_context, typed_context, Some(top_level_owners))
            .map_err(|e| match e {
                monomorphize::MonomorphizePipelineError::Monomorphize(err) => {
                    vec![CompileError::from(err)]
                }
                monomorphize::MonomorphizePipelineError::Retype(errors) => errors
                    .into_iter()
                    .map(CompileError::from)
                    .collect::<Vec<CompileError>>(),
            })?;
    Ok((resolved_context, typed_context))
}

#[cfg(test)]
#[path = "../tests/t_compile.rs"]
mod tests;
