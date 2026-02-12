//! Text formatter for SSA IR.

use super::model::*;
use crate::core::ir::{IrTypeCache, IrTypeId, IrTypeKind};
use crate::core::resolve::DefId;
use std::collections::HashMap;
use std::fmt::Write as _;

pub fn format_func(func: &Function, types: &IrTypeCache) -> String {
    let mut formatter = Formatter::new(types, false);
    formatter.write_function(func);
    formatter.finish()
}

pub fn format_func_with_comments(func: &Function, types: &IrTypeCache) -> String {
    let mut formatter = Formatter::new(types, true);
    formatter.write_function(func);
    formatter.finish()
}

pub fn format_func_with_names(
    func: &Function,
    types: &IrTypeCache,
    def_names: &HashMap<DefId, String>,
) -> String {
    let mut formatter = Formatter::new_with_names(types, false, def_names);
    formatter.write_function(func);
    formatter.finish()
}

pub fn format_func_with_comments_and_names(
    func: &Function,
    types: &IrTypeCache,
    def_names: &HashMap<DefId, String>,
) -> String {
    let mut formatter = Formatter::new_with_names(types, true, def_names);
    formatter.write_function(func);
    formatter.finish()
}

struct Formatter<'a> {
    types: &'a IrTypeCache,
    def_names: Option<&'a HashMap<DefId, String>>,
    show_comments: bool,
    out: String,
}

impl<'a> Formatter<'a> {
    fn new(types: &'a IrTypeCache, show_comments: bool) -> Self {
        Self {
            types,
            def_names: None,
            show_comments,
            out: String::new(),
        }
    }

    fn new_with_names(
        types: &'a IrTypeCache,
        show_comments: bool,
        def_names: &'a HashMap<DefId, String>,
    ) -> Self {
        Self {
            types,
            def_names: Some(def_names),
            show_comments,
            out: String::new(),
        }
    }

    fn finish(self) -> String {
        self.out
    }

    fn write_function(&mut self, func: &Function) {
        let _ = write!(&mut self.out, "fn {}(", func.name);
        for (i, param) in func.sig.params.iter().enumerate() {
            if i > 0 {
                let _ = write!(&mut self.out, ", ");
            }
            self.write_type(*param);
        }
        let _ = write!(&mut self.out, ") -> ");
        self.write_type(func.sig.ret);
        let _ = writeln!(&mut self.out, " {{");

        if !func.locals.is_empty() {
            let _ = writeln!(&mut self.out, "  locals:");
            for local in &func.locals {
                let _ = write!(&mut self.out, "    %l{}: ", local.id.0);
                self.write_type(local.ty);
                if let Some(name) = &local.name {
                    let _ = write!(&mut self.out, " // {}", name);
                }
                let _ = writeln!(&mut self.out);
            }
        }

        for (index, block) in func.blocks.iter().enumerate() {
            if index > 0 {
                let _ = writeln!(&mut self.out);
            }
            self.write_block(block);
        }

        let _ = writeln!(&mut self.out, "}}");
    }

    fn write_block(&mut self, block: &Block) {
        let _ = write!(&mut self.out, "  bb{}(", block.id.0);
        for (i, param) in block.params.iter().enumerate() {
            if i > 0 {
                let _ = write!(&mut self.out, ", ");
            }
            let _ = write!(&mut self.out, "%v{}: ", param.value.id.0);
            self.write_type(param.value.ty);
        }
        let _ = writeln!(&mut self.out, "):");

        for (idx, inst) in block.insts.iter().enumerate() {
            self.write_instruction(inst, idx > 0);
        }
        if !block.insts.is_empty() {
            let _ = writeln!(&mut self.out);
        }
        self.write_terminator(&block.term);
    }

    fn write_instruction(&mut self, inst: &Instruction, add_blank_before_comments: bool) {
        if self.show_comments && !inst.comments.is_empty() {
            if add_blank_before_comments {
                let _ = writeln!(&mut self.out);
            }
            for comment in &inst.comments {
                let _ = writeln!(&mut self.out, "    // {}", comment);
            }
        }
        let _ = write!(&mut self.out, "    ");
        if let Some(result) = &inst.result {
            let _ = write!(&mut self.out, "%v{}: ", result.id.0);
            self.write_type(result.ty);
            let _ = write!(&mut self.out, " = ");
        }
        self.write_inst_kind(&inst.kind);
        let _ = writeln!(&mut self.out);
    }

    fn write_inst_kind(&mut self, kind: &InstKind) {
        match kind {
            InstKind::Const { value } => {
                let _ = write!(&mut self.out, "const ");
                self.write_const(value);
            }
            InstKind::BinOp { op, lhs, rhs } => {
                let _ = write!(&mut self.out, "{} %v{}, %v{}", op_name(op), lhs.0, rhs.0);
            }
            InstKind::UnOp { op, value } => {
                let _ = write!(&mut self.out, "{} %v{}", unop_name(op), value.0);
            }
            InstKind::IntTrunc { value, ty } => {
                let _ = write!(&mut self.out, "trunc %v{} to ", value.0);
                self.write_type(*ty);
            }
            InstKind::IntExtend { value, ty, signed } => {
                let kind = if *signed { "sext" } else { "zext" };
                let _ = write!(&mut self.out, "{} %v{} to ", kind, value.0);
                self.write_type(*ty);
            }
            InstKind::Cmp { op, lhs, rhs } => {
                let _ = write!(
                    &mut self.out,
                    "cmp.{} %v{}, %v{}",
                    cmp_name(op),
                    lhs.0,
                    rhs.0
                );
            }
            InstKind::Cast { kind, value, ty } => {
                let kind = match kind {
                    CastKind::PtrToInt => "ptr_to_int",
                    CastKind::IntToPtr => "int_to_ptr",
                    CastKind::PtrToPtr => "ptr",
                };
                let _ = write!(&mut self.out, "cast.{} %v{} to ", kind, value.0);
                self.write_type(*ty);
            }
            InstKind::AddrOfLocal { local } => {
                let _ = write!(&mut self.out, "addr_of %l{}", local.0);
            }
            InstKind::FieldAddr { base, index } => {
                let _ = write!(&mut self.out, "field_addr %v{}, {}", base.0, index);
            }
            InstKind::IndexAddr { base, index } => {
                let _ = write!(&mut self.out, "index_addr %v{}, %v{}", base.0, index.0);
            }
            InstKind::Load { ptr } => {
                let _ = write!(&mut self.out, "load %v{}", ptr.0);
            }
            InstKind::Store { ptr, value } => {
                let _ = write!(&mut self.out, "store %v{}, %v{}", ptr.0, value.0);
            }
            InstKind::MemCopy { dst, src, len } => {
                let _ = write!(
                    &mut self.out,
                    "memcpy %v{}, %v{}, %v{}",
                    dst.0, src.0, len.0
                );
            }
            InstKind::MemSet { dst, byte, len } => {
                let _ = write!(
                    &mut self.out,
                    "memset %v{}, %v{}, %v{}",
                    dst.0, byte.0, len.0
                );
            }
            InstKind::Call { callee, args } => {
                let _ = write!(&mut self.out, "call ");
                self.write_callee(callee);
                let _ = write!(&mut self.out, "(");
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(&mut self.out, ", ");
                    }
                    let _ = write!(&mut self.out, "%v{}", arg.0);
                }
                let _ = write!(&mut self.out, ")");
            }
            InstKind::Drop { ptr } => {
                let _ = write!(&mut self.out, "drop %v{}", ptr.0);
            }
        }
    }

    fn write_terminator(&mut self, term: &Terminator) {
        let _ = write!(&mut self.out, "    ");
        match term {
            Terminator::Br { target, args } => {
                let _ = write!(&mut self.out, "br bb{}", target.0);
                self.write_block_args(args);
            }
            Terminator::CondBr {
                cond,
                then_bb,
                then_args,
                else_bb,
                else_args,
            } => {
                let _ = write!(&mut self.out, "cbr %v{}, bb{}", cond.0, then_bb.0);
                self.write_block_args(then_args);
                let _ = write!(&mut self.out, ", bb{}", else_bb.0);
                self.write_block_args(else_args);
            }
            Terminator::Switch {
                value,
                cases,
                default,
                default_args,
            } => {
                let _ = writeln!(&mut self.out, "switch %v{} {{", value.0);
                for case in cases {
                    let _ = write!(&mut self.out, "      case ");
                    self.write_const(&case.value);
                    let _ = write!(&mut self.out, " -> bb{}", case.target.0);
                    self.write_block_args(&case.args);
                    let _ = writeln!(&mut self.out);
                }
                let _ = write!(&mut self.out, "      default -> bb{}", default.0);
                self.write_block_args(default_args);
                let _ = writeln!(&mut self.out);
                let _ = writeln!(&mut self.out, "    }}");
                return;
            }
            Terminator::Return { value } => {
                let _ = write!(&mut self.out, "ret");
                if let Some(value) = value {
                    let _ = write!(&mut self.out, " %v{}", value.0);
                }
            }
            Terminator::Unreachable => {
                let _ = write!(&mut self.out, "unreachable");
            }
        }
        let _ = writeln!(&mut self.out);
    }

    fn write_block_args(&mut self, args: &[ValueId]) {
        if args.is_empty() {
            return;
        }
        let _ = write!(&mut self.out, "(");
        for (i, arg) in args.iter().enumerate() {
            if i > 0 {
                let _ = write!(&mut self.out, ", ");
            }
            let _ = write!(&mut self.out, "%v{}", arg.0);
        }
        let _ = write!(&mut self.out, ")");
    }

    fn write_callee(&mut self, callee: &Callee) {
        match callee {
            Callee::Direct(def_id) => {
                if let Some(def_names) = self.def_names
                    && let Some(name) = def_names.get(def_id)
                {
                    let _ = write!(&mut self.out, "{}", name);
                } else {
                    let _ = write!(&mut self.out, "@{}", def_id);
                }
            }
            Callee::Value(value) => {
                let _ = write!(&mut self.out, "%v{}", value.0);
            }
            Callee::Runtime(func) => {
                let _ = write!(&mut self.out, "{}", func.name());
            }
        }
    }

    fn write_const(&mut self, value: &ConstValue) {
        match value {
            ConstValue::Unit => {
                let _ = write!(&mut self.out, "()");
            }
            ConstValue::Bool(value) => {
                let _ = write!(&mut self.out, "{}", value);
            }
            ConstValue::Int { value, .. } => {
                let _ = write!(&mut self.out, "{}", value);
            }
            ConstValue::GlobalAddr { id } => {
                let _ = write!(&mut self.out, "@g{}", id.0);
            }
            ConstValue::FuncAddr { def } => {
                if let Some(def_names) = self.def_names
                    && let Some(name) = def_names.get(def)
                {
                    let _ = write!(&mut self.out, "{}", name);
                } else {
                    let _ = write!(&mut self.out, "@{}", def);
                }
            }
        }
    }

    fn write_type(&mut self, ty: IrTypeId) {
        let info = self.types.get(ty);
        if let Some(name) = &info.name {
            let _ = write!(&mut self.out, "{}", name);
            return;
        }
        match &info.kind {
            IrTypeKind::Unit => {
                let _ = write!(&mut self.out, "()");
            }
            IrTypeKind::Bool => {
                let _ = write!(&mut self.out, "bool");
            }
            IrTypeKind::Int { signed, bits } => {
                let prefix = if *signed { "i" } else { "u" };
                let _ = write!(&mut self.out, "{}{}", prefix, bits);
            }
            IrTypeKind::Ptr { elem } => {
                let _ = write!(&mut self.out, "ptr<");
                self.write_type(*elem);
                let _ = write!(&mut self.out, ">");
            }
            IrTypeKind::Array { elem, dims } => {
                self.write_type(*elem);
                let _ = write!(&mut self.out, "[");
                for (i, dim) in dims.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(&mut self.out, ", ");
                    }
                    let _ = write!(&mut self.out, "{}", dim);
                }
                let _ = write!(&mut self.out, "]");
            }
            IrTypeKind::Tuple { fields } => {
                let _ = write!(&mut self.out, "(");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(&mut self.out, ", ");
                    }
                    self.write_type(*field);
                }
                let _ = write!(&mut self.out, ")");
            }
            IrTypeKind::Struct { fields } => {
                let _ = write!(&mut self.out, "struct {{ ");
                for (i, field) in fields.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(&mut self.out, ", ");
                    }
                    let _ = write!(&mut self.out, "{}: ", field.name);
                    self.write_type(field.ty);
                }
                let _ = write!(&mut self.out, " }}");
            }
            IrTypeKind::Blob { size, align } => {
                let _ = write!(&mut self.out, "blob<{}, align={}>", size, align);
            }
            IrTypeKind::Fn { params, ret } => {
                let _ = write!(&mut self.out, "fn(");
                for (i, param) in params.iter().enumerate() {
                    if i > 0 {
                        let _ = write!(&mut self.out, ", ");
                    }
                    self.write_type(*param);
                }
                let _ = write!(&mut self.out, ") -> ");
                self.write_type(*ret);
            }
        }
    }
}

fn op_name(op: &BinOp) -> &'static str {
    match op {
        BinOp::Add => "add",
        BinOp::Sub => "sub",
        BinOp::Mul => "mul",
        BinOp::Div => "div",
        BinOp::Mod => "mod",
        BinOp::And => "and",
        BinOp::Or => "or",
        BinOp::Xor => "xor",
        BinOp::Shl => "shl",
        BinOp::Shr => "shr",
    }
}

fn cmp_name(op: &CmpOp) -> &'static str {
    match op {
        CmpOp::Eq => "eq",
        CmpOp::Ne => "ne",
        CmpOp::Lt => "lt",
        CmpOp::Le => "le",
        CmpOp::Gt => "gt",
        CmpOp::Ge => "ge",
    }
}

fn unop_name(op: &UnOp) -> &'static str {
    match op {
        UnOp::Neg => "neg",
        UnOp::Not => "not",
        UnOp::BitNot => "bitnot",
    }
}

#[cfg(test)]
#[path = "../../tests/ir/t_format.rs"]
mod tests;
