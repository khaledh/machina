use crate::backend::opt::cfg::PassManager;
use crate::ir::Function;

pub(super) fn assert_ir_eq(actual: impl AsRef<str>, expected: impl AsRef<str>) {
    let actual = normalize_ir(actual.as_ref());
    let expected = normalize_ir(expected.as_ref());
    assert_eq!(actual, expected);
}

fn run_cleanup(func: &mut Function) {
    let mut manager = PassManager::new();
    manager.run(std::slice::from_mut(func));
}

mod t_cleanup;

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
