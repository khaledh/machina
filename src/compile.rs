use std::collections::HashMap;
use std::path::PathBuf;

use crate::context::ParsedContext;
use crate::diag::CompileError;
use crate::elaborate;
use crate::lexer::{LexError, Lexer, Token};
use crate::liveness;
use crate::lower;
use crate::mcir;
use crate::mcir::types::GlobalSection;
use crate::normalize;
use crate::nrvo::NrvoAnalyzer;
use crate::opt;
use crate::parse::Parser;
use crate::regalloc;
use crate::resolve::{DefId, resolve};
use crate::semck::sem_check;
use crate::ssa;
use crate::targets;
use crate::targets::TargetKind;
use crate::tree::NodeIdGen;
use crate::tree::parsed::Module;
use crate::typeck::type_check;

#[derive(Debug)]
pub struct CompileOptions {
    pub dump: Option<String>,
    pub target: TargetKind,
    pub emit_ir: bool,
    pub trace_alloc: bool,
    pub backend: BackendKind,
    pub inject_prelude: bool,
}

#[derive(Copy, Clone, Debug, clap::ValueEnum)]
pub enum BackendKind {
    Legacy,
    Ssa,
}

pub struct CompileOutput {
    pub asm: String,
    pub ir: Option<String>,
}

pub fn compile(source: &str, opts: &CompileOptions) -> Result<CompileOutput, Vec<CompileError>> {
    // Parse dump flags from comma-separated list, e.g. --dump ast,ir,liveness
    let mut dump_tokens = false;
    let mut dump_ast = false;
    let mut dump_def_table = false;
    let mut dump_type_map = false;
    let mut dump_nrvo = false;
    let mut dump_ir = false;
    let mut dump_liveness = false;
    let mut dump_intervals = false;
    let mut dump_regalloc = false;
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
                "liveness" => dump_liveness = true,
                "intervals" => dump_intervals = true,
                "regalloc" => dump_regalloc = true,
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

    let id_gen = NodeIdGen::new();

    let (module, id_gen) = if opts.inject_prelude {
        // load stdlib/prelude_decl.mc
        let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
            .join("stdlib")
            .join("prelude_decl.mc");
        let prelude_src = std::fs::read_to_string(&prelude_path)
            .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;

        let (prelude_module, id_gen) = parse_with_id_gen(&prelude_src, id_gen)?;
        let (user_module, id_gen) = parse_with_id_gen(source, id_gen)?;

        // combine top_level_items: prelude first, then user
        let mut top_level_items = Vec::new();
        top_level_items.extend(prelude_module.top_level_items);
        top_level_items.extend(user_module.top_level_items);
        (Module { top_level_items }, id_gen)
    } else {
        let (user_module, id_gen) = parse_with_id_gen(source, id_gen)?;
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

    let resolved_context = resolve(ast_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    if dump_def_table {
        println!("Def Map:");
        println!("--------------------------------");
        println!("{}", resolved_context.def_table);
        println!("--------------------------------");
    }

    // --- Type Check (resolved -> type-checked) ---

    let type_checked_context = type_check(resolved_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

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

    let target = match opts.target {
        TargetKind::Arm64 => targets::arm64::regs::Arm64Target::new(),
    };

    match opts.backend {
        BackendKind::Legacy => {
            // --- Lower to MCIR ---
            let lowered_context = lower::lower_ast::lower_ast(analyzed_context, opts.trace_alloc)
                .map_err(|e| vec![e.into()])?;
            if dump_ir {
                println!("MCIR:");
                println!("--------------------------------");
                for func in &lowered_context.funcs {
                    let func_name = lowered_context
                        .symbols
                        .def_names
                        .get(&func.def_id)
                        .map(|name| name.as_str())
                        .unwrap_or("<unknown>");
                    println!(
                        "{}",
                        format_mcir_body(&func.body, func_name, &lowered_context.symbols.def_names)
                    );
                    println!("--------------------------------");
                }
            }

            // --- Optimize MCIR ---
            let optimized_context = opt::cfg_free::run(lowered_context);
            let liveness_context = liveness::analyze(optimized_context);
            let optimized_context = opt::dataflow::run(liveness_context);
            let liveness_context = liveness::analyze(optimized_context);

            let ir = if opts.emit_ir {
                let mut mcir_out = String::new();
                mcir_out.push_str(&format_globals(&liveness_context.globals));
                for func in &liveness_context.funcs {
                    let func_name = liveness_context
                        .symbols
                        .def_names
                        .get(&func.def_id)
                        .map(|name| name.as_str())
                        .unwrap_or("<unknown>");
                    mcir_out.push_str(&format!(
                        "{}\n",
                        format_mcir_body(
                            &func.body,
                            func_name,
                            &liveness_context.symbols.def_names
                        )
                    ));
                    mcir_out.push('\n');
                }
                Some(mcir_out)
            } else {
                None
            };

            if dump_liveness || dump_intervals {
                // --- Dump Liveness Analysis ---
                use crate::liveness::format_liveness_map;
                use regalloc::intervals::{build_live_intervals, format_live_intervals};
                for (func, live_map) in liveness_context
                    .funcs
                    .iter()
                    .zip(liveness_context.live_maps.iter())
                {
                    let func_name = liveness_context
                        .symbols
                        .def_names
                        .get(&func.def_id)
                        .map(|name| name.as_str())
                        .unwrap_or("<unknown>");
                    if dump_liveness {
                        print!("{}", format_liveness_map(live_map, func_name));
                    }
                    if dump_intervals {
                        let intervals = build_live_intervals(&func.body, live_map);
                        print!("{}", format_live_intervals(&intervals, func_name));
                    }
                }
            }

            // --- Register Allocation ---
            let regalloc_context = regalloc::regalloc(liveness_context, &target);

            if dump_regalloc {
                for (func, alloc_result) in regalloc_context
                    .funcs
                    .iter()
                    .zip(regalloc_context.alloc_results.iter())
                {
                    let func_name = regalloc_context
                        .symbols
                        .def_names
                        .get(&func.def_id)
                        .map(|name| name.as_str())
                        .unwrap_or("<unknown>");
                    print!("{}", alloc_result.format_alloc_map(func_name, &target));
                }
            }

            // --- Codegen (Assembly) ---
            let asm = match opts.target {
                TargetKind::Arm64 => {
                    let mut codegen =
                        targets::arm64::Arm64Codegen::from_regalloc_context(&regalloc_context);
                    codegen
                        .generate()
                        .map_err(|e| vec![targets::CodegenError::from(e).into()])?
                }
            };

            if dump_asm {
                println!("ASM:");
                println!("--------------------------------");
                println!("{}", asm);
                println!("--------------------------------");
            }

            Ok(CompileOutput { asm, ir })
        }
        BackendKind::Ssa => {
            // --- Lower to SSA IR ---
            let lowered = ssa::lower::lower_module_with_opts(
                &analyzed_context.module,
                &analyzed_context.def_table,
                &analyzed_context.type_map,
                &analyzed_context.lowering_plans,
                &analyzed_context.drop_plans,
                opts.trace_alloc,
            )
            .map_err(|e| vec![e.into()])?;

            // --- Optimize SSA ---
            let mut funcs: Vec<_> = lowered.funcs.iter().map(|f| f.func.clone()).collect();
            let mut pipeline = ssa::opt::Pipeline::new();
            pipeline.run(&mut funcs);

            let mut optimized_funcs = Vec::with_capacity(lowered.funcs.len());
            for (func, lowered_func) in funcs.into_iter().zip(lowered.funcs.iter()) {
                let types = lowered_func.types.clone();
                let globals = lowered_func.globals.clone();
                optimized_funcs.push(ssa::lower::LoweredFunction {
                    func,
                    types,
                    globals,
                });
            }

            let lowered = ssa::lower::LoweredModule {
                funcs: optimized_funcs,
                globals: lowered.globals.clone(),
            };

            let ir = if opts.emit_ir {
                let mut out = String::new();
                out.push_str(&format_ssa_globals(&lowered.globals));
                for (idx, func) in lowered.funcs.iter().enumerate() {
                    if idx > 0 {
                        out.push('\n');
                    }
                    out.push_str(&ssa::model::format::formact_func_with_comments(
                        &func.func,
                        &func.types,
                    ));
                    out.push('\n');
                }
                Some(out)
            } else {
                None
            };

            let asm = ssa::codegen::emit_module_arm64(
                &lowered,
                &analyzed_context.symbols.def_names,
                &target,
            );

            if dump_asm {
                println!("ASM:");
                println!("--------------------------------");
                println!("{}", asm);
                println!("--------------------------------");
            }

            Ok(CompileOutput { asm, ir })
        }
    }
}

// --- stdlib parsed-tree injection ---

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

// --- Formatting ---

fn format_mcir_body(
    body: &mcir::FuncBody,
    name: &str,
    def_names: &HashMap<DefId, String>,
) -> String {
    use crate::mcir::LocalKind;

    let mut params = body
        .locals
        .iter()
        .enumerate()
        .filter_map(|(i, local)| match local.kind {
            LocalKind::Param { index } => Some((index, i, local)),
            _ => None,
        })
        .collect::<Vec<_>>();
    params.sort_by_key(|(index, _, _)| *index);

    let mut param_parts = Vec::new();
    for (index, _, local) in params {
        let ty_str = body.types.type_to_string(local.ty);
        let name = local.name.clone().unwrap_or_else(|| format!("p{}", index));
        param_parts.push(format!("{}: {}", name, ty_str));
    }

    let ret_ty = body
        .types
        .type_to_string(body.locals[body.ret_local.index()].ty);
    let header = format!("fn {}({}) -> {} {{", name, param_parts.join(", "), ret_ty);

    let body_str = body.to_string().replacen("body {", &header, 1);
    replace_func_addrs(&body_str, def_names)
}

fn format_globals(globals: &[mcir::types::GlobalItem]) -> String {
    if globals.is_empty() {
        return String::new();
    }
    let mut out = String::new();
    out.push_str("globals:\n");
    for g in globals {
        let kind = match g.kind {
            GlobalSection::RoData => "rodata",
            GlobalSection::RwData => "rwdata",
        };
        let payload = match &g.payload {
            crate::mcir::types::GlobalPayload::Bytes(b) => {
                let hex = b
                    .iter()
                    .map(|b| format!("0x{:02x}", b))
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", hex)
            }
            crate::mcir::types::GlobalPayload::String(s) => format!("\"{}\"", s),
        };
        out.push_str(&format!("  g#{} ({}): {}\n", g.id.index(), kind, payload));
    }
    out.push('\n');
    out
}

fn format_ssa_globals(globals: &[ssa::model::ir::GlobalData]) -> String {
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

fn replace_func_addrs(text: &str, def_names: &HashMap<DefId, String>) -> String {
    let bytes = text.as_bytes();
    let mut out = String::with_capacity(text.len());
    let mut i = 0;
    while i < bytes.len() {
        if i + 3 <= bytes.len()
            && (bytes[i] == b'f' && bytes[i + 1] == b'n' && bytes[i + 2] == b'#'
                || bytes[i] == b'd'
                    && bytes[i + 1] == b'e'
                    && bytes[i + 2] == b'f'
                    && i + 3 < bytes.len()
                    && bytes[i + 3] == b'#')
        {
            let is_fn = bytes[i] == b'f';
            let prefix_len = if is_fn { 3 } else { 4 };
            let start = i;
            i += prefix_len;
            let digits_start = i;
            while i < bytes.len() && bytes[i].is_ascii_digit() {
                i += 1;
            }
            if digits_start == i {
                out.push_str(if is_fn { "fn#" } else { "def#" });
                continue;
            }
            if let Ok(id) = text[digits_start..i].parse::<u32>() {
                let def_id = DefId(id);
                if let Some(name) = def_names.get(&def_id) {
                    if is_fn {
                        out.push('&');
                    }
                    out.push_str(name);
                    continue;
                }
            }
            out.push_str(&text[start..i]);
            continue;
        }
        out.push(bytes[i] as char);
        i += 1;
    }
    out
}
