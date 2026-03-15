//! Compact formatting helpers for semantic nodes.

use crate::core::ast::*;

pub fn format_semantic_stmt_compact(stmt: &StmtExpr) -> String {
    match &stmt.kind {
        StmtExprKind::LetBind { pattern, value, .. } => format!(
            "let {} = {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(value)
        ),
        StmtExprKind::VarBind { pattern, value, .. } => format!(
            "var {} = {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(value)
        ),
        StmtExprKind::VarDecl { ident, .. } => format!("var {ident}"),
        StmtExprKind::Assign {
            assignee, value, ..
        } => format!(
            "{} = {}",
            format_semantic_place_expr_compact(assignee),
            format_semantic_value_expr_compact(value)
        ),
        StmtExprKind::CompoundAssign {
            assignee,
            op,
            value,
            ..
        } => format!(
            "{} {}= {}",
            format_semantic_place_expr_compact(assignee),
            format_binary_op(op),
            format_semantic_value_expr_compact(value)
        ),
        StmtExprKind::While { cond, .. } => {
            format!("while {}", format_semantic_value_expr_compact(cond))
        }
        StmtExprKind::For { pattern, iter, .. } => format!(
            "for {} in {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(iter)
        ),
        StmtExprKind::Defer { value } => {
            format!("defer {}", format_semantic_value_expr_compact(value))
        }
        StmtExprKind::Using { binding, value, .. } => format!(
            "using {} = {}",
            binding.ident,
            format_semantic_value_expr_compact(value)
        ),
        StmtExprKind::Break => "break".to_string(),
        StmtExprKind::Continue => "continue".to_string(),
        StmtExprKind::Return { value } => match value {
            Some(expr) => format!("return {}", format_semantic_value_expr_compact(expr)),
            None => "return".to_string(),
        },
    }
}

pub fn format_semantic_bind_pattern_compact(pattern: &BindPattern) -> String {
    fn fmt_inner(pattern: &BindPattern, out: &mut String) {
        match &pattern.kind {
            BindPatternKind::Name { ident, .. } => out.push_str(ident),
            BindPatternKind::Array { patterns } => {
                out.push('[');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    fmt_inner(pattern, out);
                }
                out.push(']');
            }
            BindPatternKind::Tuple { patterns } => {
                out.push('(');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    fmt_inner(pattern, out);
                }
                out.push(')');
            }
            BindPatternKind::Struct { name, fields } => {
                out.push_str(name);
                out.push_str(" { ");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    out.push_str(&field.name);
                    out.push_str(": ");
                    fmt_inner(&field.pattern, out);
                }
                out.push_str(" }");
            }
        }
    }

    let mut out = String::new();
    fmt_inner(pattern, &mut out);
    out
}

pub fn format_semantic_place_expr_compact(place: &Expr) -> String {
    match &place.kind {
        ExprKind::Var { ident } => ident.clone(),
        ExprKind::Deref { expr } => {
            format!("*{}", format_semantic_value_expr_compact(expr))
        }
        ExprKind::ArrayIndex { target, indices } => {
            let mut out = format!("{}[", format_semantic_place_expr_compact(target));
            for (i, index) in indices.iter().enumerate() {
                if i > 0 {
                    out.push_str(", ");
                }
                out.push_str(&format_semantic_value_expr_compact(index));
            }
            out.push(']');
            out
        }
        ExprKind::TupleField { target, index } => {
            format!("{}.{}", format_semantic_place_expr_compact(target), index)
        }
        ExprKind::StructField { target, field } => {
            format!("{}.{}", format_semantic_place_expr_compact(target), field)
        }
        _ => "<?>".to_string(),
    }
}

pub fn format_semantic_value_expr_compact(expr: &Expr) -> String {
    match &expr.kind {
        ExprKind::Block { .. } => "{ ... }".to_string(),
        ExprKind::UnitLit => "()".to_string(),
        ExprKind::IntLit(value) => value.to_string(),
        ExprKind::BoolLit(value) => value.to_string(),
        ExprKind::CharLit(value) => fmt_char(*value),
        ExprKind::StringLit { value } => fmt_string(value),
        ExprKind::StringFmt { segments } => fmt_string_fmt(segments),
        ExprKind::ArrayLit { init, .. } => match init {
            ArrayLitInit::Elems(elems) => {
                let elems = elems
                    .iter()
                    .map(format_semantic_value_expr_compact)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", elems)
            }
            ArrayLitInit::Repeat(expr, count) => {
                format!("[{}; {}]", format_semantic_value_expr_compact(expr), count)
            }
        },
        ExprKind::SetLit { elems, .. } => {
            let elems = elems
                .iter()
                .map(format_semantic_value_expr_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", elems)
        }
        ExprKind::MapLit { entries, .. } => {
            let entries = entries
                .iter()
                .map(|entry| {
                    format!(
                        "{}: {}",
                        format_semantic_value_expr_compact(&entry.key),
                        format_semantic_value_expr_compact(&entry.value)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", entries)
        }
        ExprKind::TupleLit(items) => {
            let items = items
                .iter()
                .map(format_semantic_value_expr_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", items)
        }
        ExprKind::StructLit { name, fields, .. } => {
            let fields = fields
                .iter()
                .map(|field| {
                    format!(
                        "{}: {}",
                        field.name,
                        format_semantic_value_expr_compact(&field.value)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            format!("{name} {{ {fields} }}")
        }
        ExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
            ..
        } => {
            let payload = payload
                .iter()
                .map(format_semantic_value_expr_compact)
                .collect::<Vec<_>>()
                .join(", ");
            if payload.is_empty() {
                format!("{enum_name}::{variant}")
            } else {
                format!("{enum_name}::{variant}({payload})")
            }
        }
        ExprKind::StructUpdate { target, fields } => {
            let fields = fields
                .iter()
                .map(|field| {
                    format!(
                        "{}: {}",
                        field.name,
                        format_semantic_value_expr_compact(&field.value)
                    )
                })
                .collect::<Vec<_>>()
                .join(", ");
            let base = format_semantic_value_expr_compact(target);
            if fields.is_empty() {
                format!("{base} {{ .. }}")
            } else {
                format!("{base} {{ {fields}, .. }}")
            }
        }
        ExprKind::BinOp { left, op, right } => format!(
            "{} {} {}",
            format_semantic_value_expr_compact(left),
            format_binary_op(op),
            format_semantic_value_expr_compact(right)
        ),
        ExprKind::UnaryOp { op, expr } => {
            format!(
                "{}{}",
                format_unary_op(op),
                format_semantic_value_expr_compact(expr)
            )
        }
        ExprKind::Try {
            fallible_expr,
            on_error,
        } => {
            let fallible = format_semantic_value_expr_compact(fallible_expr);
            if let Some(handler) = on_error {
                format!(
                    "{fallible} or {}",
                    format_semantic_value_expr_compact(handler)
                )
            } else {
                format!("{fallible}?")
            }
        }
        ExprKind::HeapAlloc { expr } => {
            format!("^{}", format_semantic_value_expr_compact(expr))
        }
        ExprKind::Move { expr } => {
            format!("move {}", format_semantic_place_expr_compact(expr))
        }
        ExprKind::ImplicitMove { expr } => {
            format!("move {}", format_semantic_place_expr_compact(expr))
        }
        ExprKind::Coerce { expr, .. } => format_semantic_value_expr_compact(expr),
        ExprKind::AddrOf { expr } => {
            format!("&{}", format_semantic_place_expr_compact(expr))
        }
        ExprKind::Load { expr } => format_semantic_place_expr_compact(expr),
        ExprKind::If { cond, .. } => {
            format!("if {} {{ ... }}", format_semantic_value_expr_compact(cond))
        }
        ExprKind::Range { start, end } => format!(
            "{}..{}",
            format_semantic_value_expr_compact(start),
            format_semantic_value_expr_compact(end)
        ),
        ExprKind::Slice { target, start, end } => {
            let mut out = format_semantic_place_expr_compact(target).to_string();
            out.push('[');
            if let Some(expr) = start {
                out.push_str(&format_semantic_value_expr_compact(expr));
            }
            out.push_str("..");
            if let Some(expr) = end {
                out.push_str(&format_semantic_value_expr_compact(expr));
            }
            out.push(']');
            out
        }
        ExprKind::MapGet { target, key } => format!(
            "{}[{}]",
            format_semantic_value_expr_compact(target),
            format_semantic_value_expr_compact(key)
        ),
        ExprKind::Len { expr } => {
            format!("len({})", format_semantic_place_expr_compact(expr))
        }
        ExprKind::Match { .. } => "match ...".to_string(),
        ExprKind::Call { callee, args } => {
            let args = args
                .iter()
                .map(format_semantic_call_arg_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", format_semantic_value_expr_compact(callee), args)
        }
        ExprKind::MethodCall {
            callee,
            method_name,
            args,
        } => {
            let recv = format_semantic_value_expr_compact(callee);
            let args = args
                .iter()
                .map(format_semantic_call_arg_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{recv}.{method_name}({args})")
        }
        ExprKind::Emit { kind } => match kind {
            EmitKind::Send { to, payload } => format!(
                "send({}, {})",
                format_semantic_value_expr_compact(to),
                format_semantic_value_expr_compact(payload)
            ),
            EmitKind::Request { to, payload, .. } => format!(
                "emit Request(to: {}, payload: {})",
                format_semantic_value_expr_compact(to),
                format_semantic_value_expr_compact(payload)
            ),
        },
        ExprKind::Reply { cap, value } => format!(
            "reply({}, {})",
            format_semantic_value_expr_compact(cap),
            format_semantic_value_expr_compact(value)
        ),
        ExprKind::ClosureRef { ident } => format!("closure#{ident}"),
        _ => "<?>".to_string(),
    }
}

fn format_semantic_call_arg_compact(arg: &CallArg) -> String {
    let value = format_semantic_value_expr_compact(&arg.expr);
    match arg.mode {
        CallArgMode::InOut => format!("inout {value}"),
        CallArgMode::Out => format!("out {value}"),
        CallArgMode::Move => format!("sink {value}"),
        CallArgMode::Default => value,
    }
}

fn format_binary_op(op: &BinaryOp) -> String {
    format!("{op}")
}

fn format_unary_op(op: &UnaryOp) -> String {
    format!("{op}")
}

fn fmt_string(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        for escaped in ch.escape_default() {
            out.push(escaped);
        }
    }
    out.push('"');
    out
}

fn fmt_string_fmt(segments: &[StringFmtSegment]) -> String {
    let mut out = String::from("f\"");
    for segment in segments {
        match segment {
            StringFmtSegment::Literal { value, .. } => {
                for ch in value.chars() {
                    for escaped in ch.escape_default() {
                        out.push(escaped);
                    }
                }
            }
            StringFmtSegment::Expr { expr, .. } => {
                out.push('{');
                out.push_str(&format_semantic_value_expr_compact(expr));
                out.push('}');
            }
        }
    }
    out.push('"');
    out
}

fn fmt_char(value: char) -> String {
    let mut out = String::from("'");
    for ch in value.escape_default() {
        out.push(ch);
    }
    out.push('\'');
    out
}
