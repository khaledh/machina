use crate::backend::lower::lower_func;
use crate::backend::opt::cfg_free::PassManager;
use crate::context::ParsedContext;
use crate::elaborate::elaborate;
use crate::ir::format::format_func;
use crate::lexer::{LexError, Lexer, Token};
use crate::normalize::normalize;
use crate::parse::Parser;
use crate::resolve::resolve;
use crate::semck::sem_check;
use crate::typeck::type_check;

fn analyze(source: &str) -> crate::context::SemanticContext {
    let lexer = Lexer::new(source);
    let tokens = lexer
        .tokenize()
        .collect::<Result<Vec<Token>, LexError>>()
        .expect("Failed to tokenize");

    let mut parser = Parser::new(&tokens);
    let module = parser.parse().expect("Failed to parse");
    let id_gen = parser.into_id_gen();

    let ast_context = ParsedContext::new(module, id_gen);
    let resolved_context = resolve(ast_context).expect("Failed to resolve");
    let type_checked_context = type_check(resolved_context).expect("Failed to type check");
    let normalized_context = normalize(type_checked_context);
    let sem_checked_context = sem_check(normalized_context).expect("Failed to semantic check");
    elaborate(sem_checked_context)
}

fn lower_and_optimize(source: &str) -> String {
    let ctx = analyze(source);
    let func_def = ctx.module.func_defs()[0];
    let mut lowered = lower_func(
        func_def,
        &ctx.def_table,
        &ctx.type_map,
        &ctx.lowering_plans,
        &ctx.drop_plans,
    )
    .expect("failed to lower");

    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(&mut lowered.func));
    format_func(&lowered.func, &lowered.types)
}

pub(super) fn assert_ir_eq(actual: impl AsRef<str>, expected: impl AsRef<str>) {
    let actual = normalize_ir(actual.as_ref());
    let expected = normalize_ir(expected.as_ref());
    assert_eq!(actual, expected);
}

mod t_const_fold;
mod t_empty_string_print;
mod t_field_addr_cse;
mod t_index_addr_simplify;
mod t_load_cse;
mod t_local_addr_copy_elim;
mod t_local_load_forward;
mod t_store_field_addr_simplify;

fn normalize_ir(text: &str) -> String {
    let mut out = Vec::new();
    for line in text.lines() {
        let trimmed = line.trim_end();
        if trimmed.is_empty() {
            continue;
        }
        let trimmed = strip_const_suffix(trimmed);
        let trimmed = strip_case_suffix(&trimmed);
        out.push(strip_named_call_at(&trimmed));
    }
    out.join("\n")
}

fn strip_const_suffix(line: &str) -> String {
    strip_token_suffix(line, "const ")
}

fn strip_case_suffix(line: &str) -> String {
    strip_token_suffix(line, "case ")
}

fn strip_token_suffix(line: &str, needle: &str) -> String {
    let Some(token_idx) = line.find(needle) else {
        return line.to_string();
    };
    let prefix = &line[..token_idx + needle.len()];
    let rest = &line[token_idx + needle.len()..];
    let token_end = rest.find(char::is_whitespace).unwrap_or_else(|| rest.len());
    let token = &rest[..token_end];
    let Some(colon_pos) = token.find(':') else {
        return line.to_string();
    };
    let value = &token[..colon_pos];
    let mut out = String::with_capacity(line.len());
    out.push_str(prefix);
    out.push_str(value);
    out.push_str(&rest[token_end..]);
    out
}

fn strip_named_call_at(line: &str) -> String {
    let Some(call_idx) = line.find("call @") else {
        return line.to_string();
    };
    let next = line.as_bytes().get(call_idx + "call @".len());
    let Some(next) = next else {
        return line.to_string();
    };
    let next = *next as char;
    if !matches!(next, 'a'..='z' | 'A'..='Z' | '_') {
        return line.to_string();
    }
    let mut out = String::with_capacity(line.len());
    out.push_str(&line[..call_idx + "call ".len()]);
    out.push_str(&line[call_idx + "call @".len()..]);
    out
}
