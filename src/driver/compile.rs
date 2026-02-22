use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use crate::core::api::{
    FrontendPolicy, ParseModuleError, ParseModuleOptions, ResolveInputs, elaborate_stage,
    parse_module_with_id_gen_and_options, semcheck_stage,
};
use crate::core::backend;
use crate::core::backend::regalloc::arm64::Arm64Target;
use crate::core::capsule::compose::{flatten_capsule, merge_modules};
use crate::core::capsule::{self, ModuleId};
use crate::core::context::{
    AnalyzedContext, CapsuleParsedContext, ParsedContext, ResolvedContext, TypeCheckedContext,
};
use crate::core::diag::CompileError;
use crate::core::ir::format::{format_func_with_comments_and_names, format_globals};
use crate::core::lexer::{LexError, Lexer, Token};
use crate::core::monomorphize;
use crate::core::nrvo::NrvoAnalyzer;
use crate::core::resolve::DefId;
use crate::core::tree::{Module, NodeId, NodeIdGen};

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

#[derive(Debug, Clone, Copy, Default)]
struct DumpFlags {
    tokens: bool,
    ast: bool,
    def_table: bool,
    type_map: bool,
    nrvo: bool,
    ir: bool,
    asm: bool,
}

impl DumpFlags {
    fn from_spec(spec: Option<&str>) -> Self {
        let mut flags = Self::default();
        if let Some(spec) = spec {
            for item in spec.split(',').map(|s| s.trim().to_lowercase()) {
                match item.as_str() {
                    "tokens" => flags.tokens = true,
                    "ast" => flags.ast = true,
                    "deftab" => flags.def_table = true,
                    "typemap" => flags.type_map = true,
                    "nrvo" => flags.nrvo = true,
                    "ir" => flags.ir = true,
                    "asm" => flags.asm = true,
                    "" => {}
                    _ => eprintln!("[WARN] unknown dump flag: {item}"),
                }
            }
        }
        flags
    }
}

#[derive(Debug, Clone, Copy)]
struct FrontendBuildOptions {
    inject_prelude: bool,
    experimental_typestate: bool,
}

struct ParsedStageOutput {
    module: Module,
    id_gen: NodeIdGen,
    top_level_owners: HashMap<NodeId, ModuleId>,
}

struct TypedStageOutput {
    resolved_context: ResolvedContext,
    typed_context: TypeCheckedContext,
}

/// Run a module-aware capsule check (parse/resolve/typecheck) without backend lowering.
pub fn check_with_path(
    source: &str,
    source_path: &Path,
    inject_prelude: bool,
    experimental_typestate: bool,
) -> Result<(), Vec<CompileError>> {
    let parsed = build_frontend_input(
        source,
        Some(source_path),
        FrontendBuildOptions {
            inject_prelude,
            experimental_typestate,
        },
    )?;
    let _ = run_frontend_strict(parsed)?;
    Ok(())
}

pub fn compile(source: &str, opts: &CompileOptions) -> Result<CompileOutput, Vec<CompileError>> {
    compile_with_path(source, None, opts)
}

pub fn compile_with_path(
    source: &str,
    source_path: Option<&Path>,
    opts: &CompileOptions,
) -> Result<CompileOutput, Vec<CompileError>> {
    let dump = DumpFlags::from_spec(opts.dump.as_deref());

    dump_tokens_stage(source, dump)?;

    let parsed = build_frontend_input(
        source,
        source_path,
        FrontendBuildOptions {
            inject_prelude: opts.inject_prelude,
            experimental_typestate: opts.experimental_typestate,
        },
    )?;

    dump_ast_stage(&parsed.module, dump);

    let typed = run_frontend_strict(parsed)?;

    dump_def_table_stage(&typed.resolved_context, dump);
    dump_type_map_stage(&typed.typed_context, dump);

    let analyzed = run_semantic_stage(typed)?;

    dump_nrvo_stage(&analyzed, dump);

    let lowered = run_lower_stage(&analyzed, opts)?;
    let lowered = run_optimize_stage(lowered, &analyzed);

    verify_ir_stage(&lowered, opts.verify_ir)?;

    let codegen_def_names = build_codegen_name_map(&analyzed, &lowered);
    let formatted_ir = format_ir_stage(&lowered, &codegen_def_names, opts.emit_ir || dump.ir);

    dump_ir_stage(formatted_ir.as_deref(), dump);

    let ir = if opts.emit_ir { formatted_ir } else { None };

    let target = Arm64Target::new();
    let asm = backend::codegen::emit_module_arm64(&lowered, &codegen_def_names, &target);

    dump_asm_stage(&asm, dump);

    Ok(CompileOutput { asm, ir })
}

fn dump_tokens_stage(source: &str, dump: DumpFlags) -> Result<(), Vec<CompileError>> {
    if !dump.tokens {
        return Ok(());
    }

    let tokens = Lexer::new(source)
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .map_err(|e| vec![e.into()])?;

    println!("Tokens:");
    println!("--------------------------------");
    for (i, token) in tokens.iter().enumerate() {
        println!("{}: {}", i, token);
    }
    println!("--------------------------------");

    Ok(())
}

fn dump_ast_stage(module: &Module, dump: DumpFlags) {
    if !dump.ast {
        return;
    }

    println!("Parsed Tree:");
    println!("--------------------------------");
    println!("{}", module);
    println!("--------------------------------");
}

fn dump_def_table_stage(resolved_context: &ResolvedContext, dump: DumpFlags) {
    if !dump.def_table {
        return;
    }

    println!("Def Map:");
    println!("--------------------------------");
    println!("{}", resolved_context.def_table);
    println!("--------------------------------");
}

fn dump_type_map_stage(type_checked_context: &TypeCheckedContext, dump: DumpFlags) {
    if !dump.type_map {
        return;
    }

    println!("Type Map:");
    println!("--------------------------------");
    println!("{}", type_checked_context.type_map);
    println!("--------------------------------");
}

fn dump_nrvo_stage(analyzed: &AnalyzedContext, dump: DumpFlags) {
    if !dump.nrvo {
        return;
    }

    println!("NRVO:");
    println!("--------------------------------");
    for def in analyzed.def_table.get_nrvo_eligible_defs() {
        println!("{}", def);
    }
    println!("--------------------------------");
}

fn dump_ir_stage(formatted_ir: Option<&str>, dump: DumpFlags) {
    if !dump.ir {
        return;
    }

    if let Some(ir) = formatted_ir {
        println!("SSA IR:");
        println!("--------------------------------");
        println!("{ir}");
        println!("--------------------------------");
    }
}

fn dump_asm_stage(asm: &str, dump: DumpFlags) {
    if !dump.asm {
        return;
    }

    println!("ASM:");
    println!("--------------------------------");
    println!("{}", asm);
    println!("--------------------------------");
}

fn build_frontend_input(
    source: &str,
    source_path: Option<&Path>,
    opts: FrontendBuildOptions,
) -> Result<ParsedStageOutput, Vec<CompileError>> {
    let (user_module, id_gen, top_level_owners) = if let Some(path) = source_path {
        let capsule = capsule::discover_and_parse_capsule_with_options(
            source,
            path,
            capsule::CapsuleParseOptions {
                experimental_typestate: opts.experimental_typestate,
            },
        )
        .map_err(|e| vec![e.into()])?;
        let program_context = CapsuleParsedContext::new(capsule);
        let flattened = flatten_capsule(&program_context)
            .map_err(|errs| errs.into_iter().map(CompileError::from).collect::<Vec<_>>())?;
        (
            flattened.module,
            program_context.next_node_id_gen().clone(),
            flattened.top_level_owners,
        )
    } else {
        let id_gen = NodeIdGen::new();
        let (module, id_gen) = parse_with_id_gen(source, id_gen, opts.experimental_typestate)?;
        (module, id_gen, HashMap::new())
    };

    let (module, id_gen) = if opts.inject_prelude {
        inject_prelude_module(user_module, id_gen, opts.experimental_typestate)?
    } else {
        (user_module, id_gen)
    };

    Ok(ParsedStageOutput {
        module,
        id_gen,
        top_level_owners,
    })
}

fn inject_prelude_module(
    user_module: Module,
    id_gen: NodeIdGen,
    experimental_typestate: bool,
) -> Result<(Module, NodeIdGen), Vec<CompileError>> {
    let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("std")
        .join("prelude_decl.mc");
    let prelude_src = std::fs::read_to_string(&prelude_path)
        .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;
    let (prelude_module, id_gen) = parse_with_id_gen(&prelude_src, id_gen, experimental_typestate)?;
    Ok((merge_modules(&prelude_module, &user_module), id_gen))
}

fn run_frontend_strict(parsed: ParsedStageOutput) -> Result<TypedStageOutput, Vec<CompileError>> {
    let ast_context = ParsedContext::new(parsed.module, parsed.id_gen);
    let (resolved_context, typed_context) =
        resolve_and_typecheck_strict(ast_context, &parsed.top_level_owners)?;

    Ok(TypedStageOutput {
        resolved_context,
        typed_context,
    })
}

fn run_semantic_stage(typed: TypedStageOutput) -> Result<AnalyzedContext, Vec<CompileError>> {
    let semantic_checked_context = semcheck_stage(typed.typed_context).map_err(|errs| {
        errs.into_iter()
            .map(Into::into)
            .collect::<Vec<CompileError>>()
    })?;

    let elaborated_context = elaborate_stage(semantic_checked_context);
    Ok(NrvoAnalyzer::new(elaborated_context).analyze())
}

fn run_lower_stage(
    analyzed: &AnalyzedContext,
    opts: &CompileOptions,
) -> Result<backend::lower::LoweredModule, Vec<CompileError>> {
    backend::lower::lower_module_with_opts(
        &analyzed.module,
        &analyzed.def_table,
        &analyzed.type_map,
        &analyzed.lowering_plans,
        &analyzed.drop_plans,
        &backend::lower::LowerOpts {
            machine_plans: Some(&analyzed.machine_plans),
            trace_alloc: opts.trace_alloc,
            trace_drops: opts.trace_drops,
            executable: true,
        },
    )
    .map_err(|e| vec![e.into()])
}

fn run_optimize_stage(
    lowered: backend::lower::LoweredModule,
    analyzed: &AnalyzedContext,
) -> backend::lower::LoweredModule {
    let mut funcs: Vec<_> = lowered.funcs.iter().map(|f| f.func.clone()).collect();
    let skip_opt = std::env::var("MACHINA_DISABLE_SSA_OPT").ok().is_some();

    let reachable = if skip_opt {
        None
    } else {
        let mut pipeline = backend::opt::Pipeline::new();
        pipeline.run(&mut funcs);
        Some(backend::opt::module_dce::reachable_def_ids(&funcs))
    };

    let machine_handler_defs: HashSet<_> = analyzed.machine_plans.thunks.keys().copied().collect();
    let mut optimized_funcs = Vec::with_capacity(lowered.funcs.len());

    for (func, lowered_func) in funcs.into_iter().zip(lowered.funcs.iter()) {
        let reachable_keep = reachable
            .as_ref()
            .map(|defs| defs.contains(&func.def_id))
            .unwrap_or(true);
        // Keep managed dispatch thunks alive even when not yet reachable from
        // `main`; runtime bootstrap consumes them via descriptor tables.
        let machine_thunk_keep = func.name.starts_with("__mc_machine_dispatch_thunk_");
        // Keep generated typestate handler defs because dispatch thunks invoke
        // them indirectly via function-address constants.
        let machine_handler_keep = machine_handler_defs.contains(&func.def_id);

        if reachable_keep || machine_thunk_keep || machine_handler_keep {
            optimized_funcs.push(backend::lower::LoweredFunction {
                func,
                types: lowered_func.types.clone(),
                globals: lowered_func.globals.clone(),
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

    lowered
}

fn verify_ir_stage(
    lowered: &backend::lower::LoweredModule,
    verify_ir: bool,
) -> Result<(), Vec<CompileError>> {
    if !verify_ir {
        return Ok(());
    }
    backend::verify::verify_module(lowered).map_err(|e| vec![e.into()])
}

fn build_codegen_name_map(
    analyzed: &AnalyzedContext,
    lowered: &backend::lower::LoweredModule,
) -> HashMap<DefId, String> {
    // Synthetic managed-machine artifacts are injected after resolve, so they
    // do not exist in resolver symbol tables by default. Seed codegen names
    // from lowered IR function names to preserve required runtime symbols
    // (notably `__mc_machine_bootstrap` weak-hook override).
    let mut codegen_def_names = analyzed.symbols.def_names.clone();
    for f in &lowered.funcs {
        if f.func.name == "__mc_user_main" {
            codegen_def_names.insert(f.func.def_id, "__mc_user_main".to_string());
        }
        if f.func.name == "__mc_entry_main_wrapper" {
            codegen_def_names.insert(f.func.def_id, "main".to_string());
        }
        if f.func.name.starts_with("__mc_machine_") {
            codegen_def_names
                .entry(f.func.def_id)
                .or_insert_with(|| f.func.name.clone());
        }
    }
    codegen_def_names
}

fn format_ir_stage(
    lowered: &backend::lower::LoweredModule,
    codegen_def_names: &HashMap<DefId, String>,
    enabled: bool,
) -> Option<String> {
    if !enabled {
        return None;
    }

    let mut out = String::new();
    out.push_str(&format_globals(&lowered.globals));
    for (idx, func) in lowered.funcs.iter().enumerate() {
        if idx > 0 {
            out.push('\n');
        }
        out.push_str(&format_func_with_comments_and_names(
            &func.func,
            &func.types,
            codegen_def_names,
        ));
    }
    Some(out)
}

// --- std parsed-tree injection ---

fn parse_with_id_gen(
    source: &str,
    id_gen: NodeIdGen,
    experimental_typestate: bool,
) -> Result<(Module, NodeIdGen), Vec<CompileError>> {
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
) -> Result<(ResolvedContext, TypeCheckedContext), Vec<CompileError>> {
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
