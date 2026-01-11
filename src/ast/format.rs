use super::*;

use std::fmt;

impl fmt::Display for Module {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        for (i, decl) in self.decls.iter().enumerate() {
            match decl {
                Decl::TypeDef(type_def) => type_def.fmt_with_indent(f, 0)?,
                Decl::FuncDecl(func_decl) => func_decl.fmt_with_indent(f, 0)?,
                Decl::FuncDef(func_def) => func_def.fmt_with_indent(f, 0)?,
                Decl::MethodBlock(method_block) => method_block.fmt_with_indent(f, 0)?,
                Decl::ClosureDecl(closure_decl) => closure_decl.fmt_with_indent(f, 0)?,
            }
            if i + 1 != self.decls.len() {
                writeln!(f, "--------------------------------")?;
            }
        }
        Ok(())
    }
}

impl TypeDef {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}TypeDef [{}]", pad, self.id)?;
        let pad1 = indent(level + 1);
        writeln!(f, "{}Name: {}", pad1, self.name)?;
        writeln!(f, "{}Kind:", pad1)?;
        self.kind.fmt_with_indent(f, level + 2)?;
        Ok(())
    }
}

impl TypeDefKind {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            TypeDefKind::Alias { aliased_ty } => {
                writeln!(f, "{}Alias: {} [{}]", pad, aliased_ty, aliased_ty.id)?;
            }
            TypeDefKind::Struct { fields } => {
                writeln!(f, "{}Struct:", pad)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 2)?;
                }
            }
            TypeDefKind::Enum { variants } => {
                writeln!(f, "{}Enum:", pad)?;
                for variant in variants {
                    if variant.payload.is_empty() {
                        writeln!(f, "{}- {} [{}]", pad, variant.name, variant.id)?;
                    } else {
                        let payload_str = variant
                            .payload
                            .iter()
                            .map(|p| p.to_string())
                            .collect::<Vec<_>>();
                        writeln!(
                            f,
                            "{}- {}({}) [{}]",
                            pad,
                            variant.name,
                            payload_str.join(", "),
                            variant.id
                        )?;
                    }
                }
            }
        }
        Ok(())
    }
}

impl ArrayLitInit {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            ArrayLitInit::Elems(elems) => {
                let elems_str = elems
                    .iter()
                    .map(|e| format!("{}", e))
                    .collect::<Vec<_>>()
                    .join(", ");
                writeln!(f, "{}Elems: [{}]", pad, elems_str)?;
            }
            ArrayLitInit::Repeat(expr, _) => {
                writeln!(f, "{}Repeat: {}", pad, expr)?;
            }
        }
        Ok(())
    }
}

impl StructField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.ty, self.id)?;
        Ok(())
    }
}

impl StructLitField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.value, self.id)?;
        Ok(())
    }
}

impl StructUpdateField {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}: {} [{}]", pad1, self.name, self.value, self.id)?;
        Ok(())
    }
}

impl FunctionSig {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}Name: {}", pad, self.name)?;
        writeln!(f, "{}Return Type: {}", pad, self.return_type)?;
        writeln!(f, "{}Params:", pad)?;
        for param in &self.params {
            writeln!(f, "{}{}", indent(level + 2), param)?;
        }
        Ok(())
    }
}

impl FuncDecl {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}FuncDecl [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)?;
        Ok(())
    }
}

impl FuncDef {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}FuncDef [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)?;
        self.body.fmt_with_indent(f, level + 1)
    }
}

impl MethodBlock {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}MethodBlock [{}]", pad, self.id)?;
        let pad1 = indent(level + 1);
        writeln!(f, "{}Type: {}", pad1, self.type_name)?;
        writeln!(f, "{}Methods:", pad1)?;
        for method in &self.method_defs {
            method.fmt_with_indent(f, level + 2)?;
        }
        Ok(())
    }
}

impl MethodDef {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}MethodDef [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)?;
        self.body.fmt_with_indent(f, level + 1)
    }
}

impl MethodSig {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}Name: {}", pad, self.name)?;
        writeln!(f, "{}Self Mode: {:?}", pad, self.self_param.mode)?;
        writeln!(f, "{}Return Type: {}", pad, self.return_type)?;
        writeln!(f, "{}Params:", pad)?;
        for param in &self.params {
            writeln!(f, "{}{}", indent(level + 2), param)?;
        }
        Ok(())
    }
}

impl ClosureDecl {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}ClosureDecl [{}]", pad, self.id)?;
        self.sig.fmt_with_indent(f, level + 1)
    }
}

impl ClosureSig {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}Name: {}", pad, self.name)?;
        writeln!(f, "{}Return Type: {}", pad, self.return_ty)?;
        writeln!(f, "{}Params:", pad)?;
        for param in &self.params {
            writeln!(f, "{}{}", indent(level + 2), param)?;
        }
        Ok(())
    }
}

impl fmt::Display for FuncDef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for Param {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.mode {
            ParamMode::In => {}
            ParamMode::InOut => {
                write!(f, "inout ")?;
            }
            ParamMode::Out => {
                write!(f, "out ")?;
            }
            ParamMode::Sink => {
                write!(f, "sink ")?;
            }
        }
        write!(f, "{}: {} [{}]", self.ident, self.typ, self.id)?;
        Ok(())
    }
}

impl fmt::Display for BindPattern {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for TypeExpr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "TypeExpr::{} [{}]", self.kind, self.id)?;
        Ok(())
    }
}

impl fmt::Display for TypeExprKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeExprKind::Named(name) => {
                write!(f, "Named({})", name)?;
            }
            TypeExprKind::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "Array({}, dims=[{}])", elem_ty, dims_str.join(", "))?;
            }
            TypeExprKind::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "Tuple([{}])", fields_str.join(", "))?;
            }
            TypeExprKind::Range { min, max } => {
                write!(f, "Range({}, {})", min, max)?;
            }
            TypeExprKind::Slice { elem_ty } => {
                write!(f, "Slice({})", elem_ty)?;
            }
            TypeExprKind::Heap { elem_ty } => {
                write!(f, "Heap({})", elem_ty)?;
            }
            TypeExprKind::Fn { params, return_ty } => {
                let params_str = params
                    .iter()
                    .map(|param| {
                        let mode = match param.mode {
                            ParamMode::In => "",
                            ParamMode::InOut => "inout ",
                            ParamMode::Out => "out ",
                            ParamMode::Sink => "sink ",
                        };
                        format!("{}{}", mode, param.ty)
                    })
                    .collect::<Vec<_>>()
                    .join(", ");
                write!(f, "Fn([{}] -> {})", params_str, return_ty)?;
            }
        }
        Ok(())
    }
}

impl BindPattern {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            BindPatternKind::Name(ident) => {
                writeln!(f, "{}Name({}) [{}]", pad, ident, self.id)?;
            }
            BindPatternKind::Array { patterns } => {
                writeln!(f, "{}Array [{}]", pad, self.id)?;
                for pattern in patterns {
                    pattern.fmt_with_indent(f, level + 1)?;
                }
            }
            BindPatternKind::Tuple { patterns } => {
                writeln!(f, "{}Tuple [{}]", pad, self.id)?;
                for pattern in patterns {
                    pattern.fmt_with_indent(f, level + 1)?;
                }
            }
            BindPatternKind::Struct { name, fields } => {
                writeln!(f, "{}Struct({}) [{}]", pad, name, self.id)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 1)?;
                }
            }
        }
        Ok(())
    }
}

impl StructFieldBindPattern {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        writeln!(f, "{}{}:", pad1, self.name)?;
        self.pattern.fmt_with_indent(f, level + 2)?;
        Ok(())
    }
}

impl MatchArm {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        writeln!(f, "{}MatchArm [{}]", pad, self.id)?;
        self.pattern.fmt_with_indent(f, level + 1)?;
        writeln!(f, "{}Body:", pad)?;
        self.body.fmt_with_indent(f, level + 1)?;
        Ok(())
    }
}

impl MatchPattern {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            MatchPattern::Wildcard { .. } => {
                writeln!(f, "{}Wildcard", pad)?;
            }
            MatchPattern::BoolLit { value, .. } => {
                writeln!(f, "{}BoolLit({})", pad, value)?;
            }
            MatchPattern::IntLit { value, .. } => {
                writeln!(f, "{}IntLit({})", pad, value)?;
            }
            MatchPattern::Binding { id, ident, .. } => {
                writeln!(f, "{}Binding {} [{}]", pad, ident, id)?;
            }
            MatchPattern::Tuple { patterns, .. } => {
                writeln!(f, "{}Tuple", pad)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Patterns:", pad1)?;
                for pattern in patterns {
                    pattern.fmt_with_indent(f, level + 2)?;
                }
            }
            MatchPattern::EnumVariant {
                enum_name,
                variant_name,
                bindings,
                ..
            } => {
                writeln!(f, "{}EnumVariant", pad)?;
                let pad1 = indent(level + 1);
                if let Some(enum_name) = enum_name {
                    writeln!(f, "{}Enum Name: {}", pad1, enum_name)?;
                }
                writeln!(f, "{}Variant Name: {}", pad1, variant_name)?;
                writeln!(f, "{}Bindings:", pad1)?;
                for binding in bindings {
                    binding.fmt_with_indent(f, level + 2)?;
                }
            }
        }
        Ok(())
    }
}

impl MatchPatternBinding {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad1 = indent(level + 1);
        match self {
            MatchPatternBinding::Named { id, ident, .. } => {
                writeln!(f, "{}{}: [{}]", pad1, ident, id)?;
            }
            MatchPatternBinding::Wildcard { .. } => {
                writeln!(f, "{}_", pad1)?;
            }
        }
        Ok(())
    }
}

impl BlockItem {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        match self {
            BlockItem::Stmt(stmt) => stmt.fmt_with_indent(f, level),
            BlockItem::Expr(expr) => expr.fmt_with_indent(f, level),
        }
    }
}

impl StmtExpr {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            StmtExprKind::LetBind {
                pattern,
                decl_ty,
                value,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Let [{}]", pad, self.id)?;
                writeln!(f, "{}Pattern:", pad1)?;
                pattern.fmt_with_indent(f, level + 2)?;
                if let Some(decl_ty) = decl_ty {
                    writeln!(f, "{}Decl Type:", pad1)?;
                    writeln!(f, "{}{}", indent(level + 2), decl_ty)?;
                }
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::VarBind {
                pattern,
                decl_ty,
                value,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Var [{}]", pad, self.id)?;
                writeln!(f, "{}Pattern:", pad1)?;
                pattern.fmt_with_indent(f, level + 2)?;
                if let Some(decl_ty) = decl_ty {
                    writeln!(f, "{}Decl Type: {}", pad1, decl_ty)?;
                }
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::VarDecl { ident, decl_ty } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}VarDecl [{}]", pad, self.id)?;
                writeln!(f, "{}Ident: {}", pad1, ident)?;
                writeln!(f, "{}Decl Type: {}", pad1, decl_ty)?;
            }
            StmtExprKind::Assign { assignee, value } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Assign [{}]", pad, self.id)?;
                writeln!(f, "{}Assignee:", pad1)?;
                assignee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Value:", pad1)?;
                value.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::While { cond, body } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}While [{}]", pad, self.id)?;
                writeln!(f, "{}Cond:", pad1)?;
                cond.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
            }
            StmtExprKind::For {
                pattern,
                iter,
                body,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}For [{}]", pad, self.id)?;
                writeln!(f, "{}Pattern:", pad1)?;
                pattern.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Iter:", pad1)?;
                iter.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
            }
        }
        Ok(())
    }
}

impl Expr {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match &self.kind {
            ExprKind::IntLit(value) => {
                writeln!(f, "{}IntLit({}) [{}]", pad, value, self.id)?;
            }
            ExprKind::BoolLit(value) => {
                writeln!(f, "{}BoolLit({}) [{}]", pad, value, self.id)?;
            }
            ExprKind::CharLit(value) => {
                writeln!(f, "{}CharLit({}) [{}]", pad, fmt_char(*value), self.id)?;
            }
            ExprKind::StringLit { value } => {
                writeln!(f, "{}StringLit(\"{value}\") [{}]", pad, self.id)?;
            }
            ExprKind::UnitLit => {
                writeln!(f, "{}UnitLit [{}]", pad, self.id)?;
            }
            ExprKind::ArrayLit { elem_ty, init } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}ArrayLit [{}]", pad, self.id)?;
                if let Some(elem_ty) = elem_ty {
                    writeln!(f, "{}Elem Type: {}", pad1, elem_ty)?;
                }
                writeln!(f, "{}Length: {}", pad1, init.length())?;
                init.fmt_with_indent(f, level + 1)?;
            }
            ExprKind::ArrayIndex { target, indices } => {
                writeln!(f, "{}ArrayIndex [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Indices:", pad1)?;
                for index in indices {
                    index.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::TupleLit(elems) => {
                writeln!(f, "{}TupleLit [{}]", pad, self.id)?;
                for elem in elems {
                    elem.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::TupleField { target, index } => {
                writeln!(f, "{}TupleField [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}ArrayIndex: {}", pad1, index)?;
            }
            ExprKind::StructLit { name, fields } => {
                writeln!(f, "{}StructLit [{}]", pad, self.id)?;
                writeln!(f, "{}Name: {}", pad, name)?;
                writeln!(f, "{}Fields:", pad)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::StructField { target, field } => {
                writeln!(f, "{}StructField [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Field: {}", pad1, field)?;
            }
            ExprKind::MethodCall {
                callee,
                method_name,
                args,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}MethodCall [{}]", pad, self.id)?;
                writeln!(f, "{}Target:", pad1)?;
                callee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Method Name: {}", pad1, method_name)?;
                writeln!(f, "{}Args:", pad1)?;
                self.fmt_call_args(f, level + 2, args)?;
            }
            ExprKind::Closure {
                ident,
                params,
                return_ty,
                body,
                ..
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Closure [{}]", pad, self.id)?;
                writeln!(f, "{}Ident: {}", pad1, ident)?;
                if params.is_empty() {
                    writeln!(f, "{}Params: <none>", pad1)?;
                } else {
                    writeln!(f, "{}Params:", pad1)?;
                    for param in params {
                        writeln!(f, "{}{}", indent(level + 2), param)?;
                    }
                }
                writeln!(f, "{}Return Type: {}", pad1, return_ty)?;
                writeln!(f, "{}Body:", pad1)?;
                body.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::EnumVariant {
                enum_name,
                variant,
                payload,
            } => {
                writeln!(f, "{}EnumVariant [{}]", pad, self.id)?;
                let pad1 = indent(level + 1);
                writeln!(f, "{}Type: {}", pad1, enum_name)?;
                writeln!(f, "{}Variant: {}", pad1, variant)?;
                if !payload.is_empty() {
                    writeln!(f, "{}Payload:", pad1)?;
                    for expr in payload {
                        expr.fmt_with_indent(f, level + 2)?;
                    }
                }
            }
            ExprKind::StructUpdate { target, fields } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}StructUpdate [{}]", pad, self.id)?;
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Fields:", pad1)?;
                for field in fields {
                    field.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::BinOp { left, op, right } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}BinOp [{}]", pad, self.id)?;
                writeln!(f, "{}Op: {}", pad1, op)?;
                writeln!(f, "{}Left:", pad1)?;
                left.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Right:", pad1)?;
                right.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::UnaryOp { op, expr } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}UnaryOp [{}]", pad, self.id)?;
                writeln!(f, "{}Op: {}", pad1, op)?;
                writeln!(f, "{}Operand:", pad1)?;
                expr.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::HeapAlloc { expr } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}HeapAlloc [{}]", pad, self.id)?;
                writeln!(f, "{}Operand:", pad1)?;
                expr.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Move { expr } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Move [{}]", pad, self.id)?;
                writeln!(f, "{}Operand:", pad1)?;
                expr.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Block { items, tail } => {
                writeln!(f, "{}Block [{}]", pad, self.id)?;
                for item in items {
                    item.fmt_with_indent(f, level + 1)?;
                }
                if let Some(tail) = tail {
                    tail.fmt_with_indent(f, level + 1)?;
                }
            }
            ExprKind::Var(name) => {
                writeln!(f, "{}Var({}) [{}]", pad, name, self.id)?;
            }
            ExprKind::If {
                cond,
                then_body,
                else_body,
            } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}If [{}]", pad, self.id)?;
                writeln!(f, "{}Cond:", pad1)?;
                cond.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Then:", pad1)?;
                then_body.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Else:", pad1)?;
                else_body.fmt_with_indent(f, level + 2)?;
            }
            ExprKind::Call { callee: name, args } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Call: [{}]", pad, self.id)?;
                name.fmt_with_indent(f, level + 1)?;
                writeln!(f, "{}Args:", pad1)?;
                self.fmt_call_args(f, level + 2, args)?;
            }
            ExprKind::Match { scrutinee, arms } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Match [{}]", pad, self.id)?;
                writeln!(f, "{}Scrutinee:", pad1)?;
                scrutinee.fmt_with_indent(f, level + 2)?;
                writeln!(f, "{}Arms:", pad1)?;
                for arm in arms {
                    arm.fmt_with_indent(f, level + 2)?;
                }
            }
            ExprKind::Range { start, end } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Range [{}]", pad, self.id)?;
                writeln!(f, "{}Start: {}", pad1, start)?;
                writeln!(f, "{}End: {}", pad1, end)?;
            }
            ExprKind::Slice { target, start, end } => {
                let pad1 = indent(level + 1);
                writeln!(f, "{}Slice [{}]", pad, self.id)?;
                writeln!(f, "{}Target:", pad1)?;
                target.fmt_with_indent(f, level + 2)?;
                if let Some(start) = start {
                    writeln!(f, "{}Start:", pad1)?;
                    start.fmt_with_indent(f, level + 2)?;
                } else {
                    writeln!(f, "{}Start: None", pad1)?;
                }
                if let Some(end) = end {
                    writeln!(f, "{}End:", pad1)?;
                    end.fmt_with_indent(f, level + 2)?;
                } else {
                    writeln!(f, "{}End: None", pad1)?;
                }
            }
            ExprKind::StringFmt { segments } => {
                writeln!(f, "{}StringFmt [{}]", pad, self.id)?;
                for segment in segments {
                    segment.fmt_with_indent(f, level + 1)?;
                }
            }
        }
        Ok(())
    }

    fn fmt_call_args(
        &self,
        f: &mut fmt::Formatter<'_>,
        level: usize,
        args: &[CallArg],
    ) -> fmt::Result {
        for arg in args {
            let pad = indent(level);
            match arg.mode {
                CallArgMode::Default => {
                    arg.expr.fmt_with_indent(f, level)?;
                }
                CallArgMode::InOut => {
                    writeln!(f, "{}InOutArg:", pad)?;
                    arg.expr.fmt_with_indent(f, level + 1)?;
                }
                CallArgMode::Out => {
                    writeln!(f, "{}OutArg:", pad)?;
                    arg.expr.fmt_with_indent(f, level + 1)?;
                }
                CallArgMode::Move => {
                    writeln!(f, "{}MoveArg:", pad)?;
                    arg.expr.fmt_with_indent(f, level + 1)?;
                }
            }
        }
        Ok(())
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.fmt_with_indent(f, 0)
    }
}

impl fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+")?,
            BinaryOp::Sub => write!(f, "-")?,
            BinaryOp::Mul => write!(f, "*")?,
            BinaryOp::Div => write!(f, "/")?,
            BinaryOp::Mod => write!(f, "%")?,
            BinaryOp::Eq => write!(f, "==")?,
            BinaryOp::Ne => write!(f, "!=")?,
            BinaryOp::Lt => write!(f, "<")?,
            BinaryOp::Gt => write!(f, ">")?,
            BinaryOp::LtEq => write!(f, "<=")?,
            BinaryOp::GtEq => write!(f, ">=")?,
            BinaryOp::BitOr => write!(f, "|")?,
            BinaryOp::BitXor => write!(f, "^")?,
            BinaryOp::BitAnd => write!(f, "&")?,
            BinaryOp::Shl => write!(f, "<<")?,
            BinaryOp::Shr => write!(f, ">>")?,
            BinaryOp::LogicalAnd => write!(f, "&&")?,
            BinaryOp::LogicalOr => write!(f, "||")?,
        }
        Ok(())
    }
}

impl StringFmtSegment {
    fn fmt_with_indent(&self, f: &mut fmt::Formatter<'_>, level: usize) -> fmt::Result {
        let pad = indent(level);
        match self {
            StringFmtSegment::Literal { value, .. } => {
                writeln!(f, "{}Literal: {}", pad, value)
            }
            StringFmtSegment::Expr { expr, .. } => {
                writeln!(f, "{}Expr:", pad)?;
                expr.fmt_with_indent(f, level + 1)
            }
        }
    }
}

impl fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            UnaryOp::Neg => write!(f, "-")?,
            UnaryOp::LogicalNot => write!(f, "!")?,
            UnaryOp::BitNot => write!(f, "~")?,
        }
        Ok(())
    }
}

fn indent(level: usize) -> String {
    "  ".repeat(level)
}

fn fmt_char(value: char) -> String {
    let mut out = String::from("'");
    for ch in value.escape_default() {
        out.push(ch);
    }
    out.push('\'');
    out
}
