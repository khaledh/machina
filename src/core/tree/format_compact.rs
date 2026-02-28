//! Compact formatting helpers for semantic nodes.

use crate::core::tree::semantic as sem;
use crate::core::tree::{BinaryOp, ParamMode, UnaryOp};

pub fn format_semantic_stmt_compact(stmt: &sem::StmtExpr) -> String {
    match &stmt.kind {
        sem::StmtExprKind::LetBind { pattern, value, .. } => format!(
            "let {} = {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(value)
        ),
        sem::StmtExprKind::VarBind { pattern, value, .. } => format!(
            "var {} = {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(value)
        ),
        sem::StmtExprKind::VarDecl { ident, .. } => format!("var {ident}"),
        sem::StmtExprKind::Assign {
            assignee, value, ..
        } => format!(
            "{} = {}",
            format_semantic_place_expr_compact(assignee),
            format_semantic_value_expr_compact(value)
        ),
        sem::StmtExprKind::While { cond, .. } => {
            format!("while {}", format_semantic_value_expr_compact(cond))
        }
        sem::StmtExprKind::For { pattern, iter, .. } => format!(
            "for {} in {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(iter)
        ),
        sem::StmtExprKind::Defer { value } => {
            format!("defer {}", format_semantic_value_expr_compact(value))
        }
        sem::StmtExprKind::Using { pattern, value, .. } => format!(
            "using {} = {}",
            format_semantic_bind_pattern_compact(pattern),
            format_semantic_value_expr_compact(value)
        ),
        sem::StmtExprKind::Break => "break".to_string(),
        sem::StmtExprKind::Continue => "continue".to_string(),
        sem::StmtExprKind::Return { value } => match value {
            Some(expr) => format!("return {}", format_semantic_value_expr_compact(expr)),
            None => "return".to_string(),
        },
    }
}

pub fn format_semantic_bind_pattern_compact(pattern: &sem::BindPattern) -> String {
    fn fmt_inner(pattern: &sem::BindPattern, out: &mut String) {
        match &pattern.kind {
            sem::BindPatternKind::Name { ident, .. } => out.push_str(ident),
            sem::BindPatternKind::Array { patterns } => {
                out.push('[');
                for (i, pattern) in patterns.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    fmt_inner(pattern, out);
                }
                out.push(']');
            }
            sem::BindPatternKind::Tuple { fields } => {
                out.push('(');
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        out.push_str(", ");
                    }
                    fmt_inner(&field.pattern, out);
                }
                out.push(')');
            }
            sem::BindPatternKind::Struct { name, fields } => {
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

pub fn format_semantic_place_expr_compact(place: &sem::PlaceExpr) -> String {
    match &place.kind {
        sem::PlaceExprKind::Var { ident, .. } => ident.clone(),
        sem::PlaceExprKind::Deref { value } => {
            format!("*{}", format_semantic_value_expr_compact(value))
        }
        sem::PlaceExprKind::ArrayIndex { target, indices } => {
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
        sem::PlaceExprKind::TupleField { target, index } => {
            format!("{}.{}", format_semantic_place_expr_compact(target), index)
        }
        sem::PlaceExprKind::StructField { target, field } => {
            format!("{}.{}", format_semantic_place_expr_compact(target), field)
        }
    }
}

pub fn format_semantic_value_expr_compact(expr: &sem::ValueExpr) -> String {
    match &expr.kind {
        sem::ValueExprKind::Block { .. } => "{ ... }".to_string(),
        sem::ValueExprKind::UnitLit => "()".to_string(),
        sem::ValueExprKind::IntLit(value) => value.to_string(),
        sem::ValueExprKind::BoolLit(value) => value.to_string(),
        sem::ValueExprKind::CharLit(value) => fmt_char(*value),
        sem::ValueExprKind::StringLit { value } => fmt_string(value),
        sem::ValueExprKind::StringFmt { plan } => fmt_string_fmt(plan),
        sem::ValueExprKind::ArrayLit { init, .. } => match init {
            sem::ArrayLitInit::Elems(elems) => {
                let elems = elems
                    .iter()
                    .map(format_semantic_value_expr_compact)
                    .collect::<Vec<_>>()
                    .join(", ");
                format!("[{}]", elems)
            }
            sem::ArrayLitInit::Repeat(expr, count) => {
                format!("[{}; {}]", format_semantic_value_expr_compact(expr), count)
            }
        },
        sem::ValueExprKind::SetLit { elems, .. } => {
            let elems = elems
                .iter()
                .map(format_semantic_value_expr_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{{{}}}", elems)
        }
        sem::ValueExprKind::MapLit { entries, .. } => {
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
        sem::ValueExprKind::TupleLit(items) => {
            let items = items
                .iter()
                .map(format_semantic_value_expr_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("({})", items)
        }
        sem::ValueExprKind::StructLit { name, fields } => {
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
        sem::ValueExprKind::EnumVariant {
            enum_name,
            variant,
            payload,
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
        sem::ValueExprKind::StructUpdate { target, fields } => {
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
        sem::ValueExprKind::BinOp { left, op, right } => format!(
            "{} {} {}",
            format_semantic_value_expr_compact(left),
            format_binary_op(op),
            format_semantic_value_expr_compact(right)
        ),
        sem::ValueExprKind::UnaryOp { op, expr } => {
            format!(
                "{}{}",
                format_unary_op(op),
                format_semantic_value_expr_compact(expr)
            )
        }
        sem::ValueExprKind::Try {
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
        sem::ValueExprKind::HeapAlloc { expr } => {
            format!("^{}", format_semantic_value_expr_compact(expr))
        }
        sem::ValueExprKind::Move { place } => {
            format!("move {}", format_semantic_place_expr_compact(place))
        }
        sem::ValueExprKind::ImplicitMove { place } => {
            format!("move {}", format_semantic_place_expr_compact(place))
        }
        sem::ValueExprKind::Coerce { expr, .. } => format_semantic_value_expr_compact(expr),
        sem::ValueExprKind::AddrOf { place } => {
            format!("&{}", format_semantic_place_expr_compact(place))
        }
        sem::ValueExprKind::Load { place } => format_semantic_place_expr_compact(place),
        sem::ValueExprKind::If { cond, .. } => {
            format!("if {} {{ ... }}", format_semantic_value_expr_compact(cond))
        }
        sem::ValueExprKind::Range { start, end } => format!(
            "{}..{}",
            format_semantic_value_expr_compact(start),
            format_semantic_value_expr_compact(end)
        ),
        sem::ValueExprKind::Slice { target, start, end } => {
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
        sem::ValueExprKind::MapGet { target, key } => format!(
            "{}[{}]",
            format_semantic_value_expr_compact(target),
            format_semantic_value_expr_compact(key)
        ),
        sem::ValueExprKind::Len { place } => {
            format!("len({})", format_semantic_place_expr_compact(place))
        }
        sem::ValueExprKind::Match { .. } => "match ...".to_string(),
        sem::ValueExprKind::Call { callee, args } => {
            let args = args
                .iter()
                .map(format_semantic_call_arg_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{}({})", format_semantic_value_expr_compact(callee), args)
        }
        sem::ValueExprKind::MethodCall {
            receiver,
            method_name,
            args,
        } => {
            let recv = match receiver {
                sem::MethodReceiver::ValueExpr(expr) => format_semantic_value_expr_compact(expr),
                sem::MethodReceiver::PlaceExpr(place) => format_semantic_place_expr_compact(place),
            };
            let args = args
                .iter()
                .map(format_semantic_call_arg_compact)
                .collect::<Vec<_>>()
                .join(", ");
            format!("{recv}.{method_name}({args})")
        }
        sem::ValueExprKind::EmitSend { to, payload } => format!(
            "emit Send(to: {}, payload: {})",
            format_semantic_value_expr_compact(to),
            format_semantic_value_expr_compact(payload)
        ),
        sem::ValueExprKind::EmitRequest {
            to,
            payload,
            request_site_key: _,
        } => format!(
            "emit Request(to: {}, payload: {})",
            format_semantic_value_expr_compact(to),
            format_semantic_value_expr_compact(payload)
        ),
        sem::ValueExprKind::Reply { cap, value } => format!(
            "reply({}, {})",
            format_semantic_value_expr_compact(cap),
            format_semantic_value_expr_compact(value)
        ),
        sem::ValueExprKind::ClosureRef { def_id } => format!("closure#{def_id}"),
    }
}

fn format_semantic_call_arg_compact(arg: &sem::CallArg) -> String {
    let (mode, expr) = match arg {
        sem::CallArg::In { expr, .. } => (ParamMode::In, expr),
        sem::CallArg::InOut { place, .. } => {
            return format!("inout {}", format_semantic_place_expr_compact(place));
        }
        sem::CallArg::Out { place, .. } => {
            return format!("out {}", format_semantic_place_expr_compact(place));
        }
        sem::CallArg::Sink { expr, .. } => (ParamMode::Sink, expr),
    };
    let value = format_semantic_value_expr_compact(expr);
    match mode {
        ParamMode::Sink => format!("sink {value}"),
        _ => value,
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

fn fmt_string_fmt(plan: &sem::StringFmtPlan) -> String {
    let mut out = String::from("f\"");
    for segment in &plan.segments {
        match segment {
            sem::SegmentKind::LiteralBytes(value) => {
                for ch in value.chars() {
                    for escaped in ch.escape_default() {
                        out.push(escaped);
                    }
                }
            }
            sem::SegmentKind::Bool { expr } => {
                out.push('{');
                out.push_str(&format_semantic_value_expr_compact(expr));
                out.push('}');
            }
            sem::SegmentKind::Int { expr, .. } => {
                out.push('{');
                out.push_str(&format_semantic_value_expr_compact(expr));
                out.push('}');
            }
            sem::SegmentKind::StringValue { expr } => {
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
