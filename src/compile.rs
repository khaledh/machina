use std::path::PathBuf;

use crate::ast::{Module, NodeIdGen};
use crate::context::AstContext;
use crate::diag::CompileError;
use crate::lexer::{LexError, Lexer, Token};
use crate::liveness;
use crate::lower;
use crate::mcir;
use crate::mcir::types::GlobalSection;
use crate::nrvo::NrvoAnalyzer;
use crate::opt;
use crate::parser::Parser;
use crate::regalloc;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::targets;
use crate::targets::TargetKind;
use crate::typeck::type_check;

pub struct CompileOptions {
    pub dump: Option<String>,
    pub target: TargetKind,
    pub emit_mcir: bool,
    pub trace_alloc: bool,
}

pub struct CompileOutput {
    pub asm: String,
    pub mcir: Option<String>,
}

pub fn compile(source: &str, opts: &CompileOptions) -> Result<CompileOutput, Vec<CompileError>> {
    // Parse dump flags from comma-separated list, e.g. --dump ast,ir,liveness
    let mut dump_tokens = false;
    let mut dump_ast = false;
    let mut dump_def_map = false;
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
                "defmap" => dump_def_map = true,
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

    // load stdlib/prelude.mc
    let prelude_path = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("stdlib")
        .join("prelude.mc");
    let prelude_src = std::fs::read_to_string(&prelude_path)
        .map_err(|e| vec![CompileError::Io(prelude_path.clone(), e)])?;

    let (prelude_module, id_gen) = parse_with_id_gen(&prelude_src, id_gen)?;
    let (user_module, _id_gen) = parse_with_id_gen(source, id_gen)?;

    // combine decls: prelude first, then user
    let mut decls = Vec::new();
    decls.extend(prelude_module.decls);
    decls.extend(user_module.decls);
    let module = Module { decls };

    if dump_ast {
        println!("AST:");
        println!("--------------------------------");
        println!("{}", module);
        println!("--------------------------------");
    }

    // --- Resolve Defs/Uses ---

    let ast_context = AstContext::new(module);

    let resolved_context = resolve(ast_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    if dump_def_map {
        println!("Def Map:");
        println!("--------------------------------");
        println!("{}", resolved_context.def_map);
        println!("--------------------------------");
    }

    // --- Type Check ---

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

    // --- Semantic Check ---

    let semantic_checked_context = sem_check(type_checked_context).map_err(|errs| {
        errs.into_iter()
            .map(|e| e.into())
            .collect::<Vec<CompileError>>()
    })?;

    // --- NRVO Analysis ---

    let analyzed_context = NrvoAnalyzer::new(semantic_checked_context).analyze();

    if dump_nrvo {
        println!("NRVO:");
        println!("--------------------------------");
        for def in analyzed_context.def_map.get_nrvo_eligible_defs() {
            println!("{}", def);
        }
        println!("--------------------------------");
    }

    // --- Lower to MCIR ---
    let lowered_context = lower::lower_ast::lower_ast(analyzed_context, opts.trace_alloc)
        .map_err(|e| vec![e.into()])?;
    if dump_ir {
        println!("MCIR:");
        println!("--------------------------------");
        for (i, body) in lowered_context.func_bodies.iter().enumerate() {
            let func_name = lowered_context.symbols.func_name(i).unwrap_or("<unknown>");
            println!("{}", format_mcir_body(body, func_name));
            println!("--------------------------------");
        }
    }

    // --- Optimize MCIR ---
    let optimized_context = opt::cfg_free::run(lowered_context);
    let liveness_context = liveness::analyze(optimized_context);
    let optimized_context = opt::dataflow::run(liveness_context);
    let liveness_context = liveness::analyze(optimized_context);

    let mcir = if opts.emit_mcir {
        let mut mcir_out = String::new();
        mcir_out.push_str(&format_globals(&liveness_context.globals));
        for (i, body) in liveness_context.func_bodies.iter().enumerate() {
            let func_name = liveness_context.symbols.func_name(i).unwrap_or("<unknown>");
            mcir_out.push_str(&format!("{}\n", format_mcir_body(body, func_name)));
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
        for (i, body) in liveness_context.func_bodies.iter().enumerate() {
            let live_map = &liveness_context.live_maps[i];
            let func_name = liveness_context.symbols.func_name(i).unwrap_or("<unknown>");
            if dump_liveness {
                print!("{}", format_liveness_map(live_map, func_name));
            }
            if dump_intervals {
                let intervals = build_live_intervals(body, live_map);
                print!("{}", format_live_intervals(&intervals, func_name));
            }
        }
    }

    // --- Register Allocation ---
    let target = match opts.target {
        TargetKind::Arm64 => targets::arm64::regs::Arm64Target::new(),
    };
    let regalloc_context = regalloc::regalloc(liveness_context, &target);

    if dump_regalloc {
        for (i, alloc_result) in regalloc_context.alloc_results.iter().enumerate() {
            let func_name = regalloc_context.symbols.func_name(i).unwrap_or("<unknown>");
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

    Ok(CompileOutput { asm, mcir })
}

// --- stdlib AST injection ---

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

fn format_mcir_body(body: &mcir::FuncBody, name: &str) -> String {
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

    body.to_string().replacen("body {", &header, 1)
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
