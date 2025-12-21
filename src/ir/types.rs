use indexmap::IndexMap;
use std::fmt;

use crate::ast::{BinaryOp, UnaryOp};
use crate::ir::cfg::ControlFlowGraph;

// ----------- Const -----------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrConst {
    Int { value: i64, bits: u8, signed: bool },
    Bool(bool),
    Unit,
}

impl IrConst {
    pub fn int_value(&self) -> i64 {
        match self {
            IrConst::Int { value, .. } => *value,
            IrConst::Bool(value) => {
                if *value {
                    1
                } else {
                    0
                }
            }
            IrConst::Unit => 0,
        }
    }

    pub fn type_of(&self) -> IrType {
        match self {
            IrConst::Int { bits, signed, .. } => IrType::Int {
                bits: *bits,
                signed: *signed,
            },
            IrConst::Bool(_) => IrType::Bool,
            IrConst::Unit => IrType::Int {
                bits: 1,
                signed: false,
            },
        }
    }

    pub fn size_of(&self) -> usize {
        match self {
            IrConst::Int { bits, .. } => (*bits as usize).div_ceil(8),
            IrConst::Bool(_) => 1,
            IrConst::Unit => 0,
        }
    }
}

// ----------- Temp -----------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrTempId(pub(crate) u32);

impl IrTempId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct IrTemp {
    pub kind: IrTempKind,
    pub ty: IrType,
    pub debug_name: Option<String>,
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IrTempKind {
    None,
    Local,
    Param { index: u32 },
    Return,
}

// ----------- Operand -----------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrOperand {
    Temp(IrTempId),
    Const(IrConst),
}

// ----------- Address -----------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrBaseAddr {
    IrTemp(IrTempId),
    // GlobalSymbol(String),
    // Pointer(IrTempId),
}

#[derive(Debug, Clone)]
pub struct IrAddr {
    pub base: IrBaseAddr,
    pub offset: usize,
    pub ty: IrType,
}

// ----------- Type -----------

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IrType {
    // Scalar types
    Unit,
    Int {
        bits: u8,
        signed: bool,
    },
    Bool,

    // Aggregate types
    Array {
        elem_ty: Box<IrType>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<IrType>,
    },
    Struct {
        fields: Vec<StructField>,
    },
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct StructField {
    pub name: String,
    pub ty: IrType,
}

#[allow(dead_code)]
impl IrType {
    fn round_up_to_power_of_two(value: usize) -> usize {
        let mut result = 1;
        while result < value {
            result <<= 1;
        }
        result
    }

    pub fn size_of(&self) -> usize {
        match self {
            IrType::Unit => 0,
            IrType::Int { bits, .. } => (*bits as usize).div_ceil(8),
            IrType::Bool => 1,
            IrType::Array { elem_ty, dims } => {
                let total_elems: usize = dims.iter().product();
                total_elems * elem_ty.size_of()
            }
            IrType::Tuple { fields } => {
                let total_size: usize = fields.iter().map(|f| f.size_of()).sum();
                total_size
            }
            IrType::Struct { fields } => {
                let total_size: usize = fields.iter().map(|f| f.ty.size_of()).sum();
                total_size
            }
        }
    }

    pub fn align_of(&self) -> usize {
        match self {
            IrType::Unit => 1,
            IrType::Int { .. } => {
                let size = Self::round_up_to_power_of_two(self.size_of());
                match size {
                    4 => 4,
                    8 => 8,
                    _ => 16,
                }
            }
            IrType::Bool => 1,
            IrType::Array { elem_ty, .. } => (*elem_ty).align_of(),
            IrType::Tuple { fields } => fields.iter().map(|f| f.align_of()).max().unwrap_or(1),
            IrType::Struct { fields } => fields.iter().map(|f| f.ty.align_of()).max().unwrap_or(1),
        }
    }

    pub fn is_compound(&self) -> bool {
        matches!(self, IrType::Array { .. } | IrType::Tuple { .. })
    }

    pub fn tuple_field_offset(&self, index: usize) -> usize {
        match self {
            IrType::Tuple { fields } => {
                assert!(index < fields.len(), "Tuple field index out of bounds");
                fields.iter().take(index).map(|f| f.size_of()).sum()
            }
            _ => panic!("Expected tuple type"),
        }
    }

    pub fn tuple_field_type(&self, index: usize) -> IrType {
        match self {
            IrType::Tuple { fields } => fields[index].clone(),
            _ => panic!("Expected tuple type"),
        }
    }

    pub fn struct_field_offset(&self, field_name: &str) -> usize {
        match self {
            IrType::Struct { fields, .. } => {
                let mut offset = 0;
                for field in fields {
                    if field.name == field_name {
                        return offset;
                    }
                    offset += field.ty.size_of();
                }
                panic!("Field not found in struct");
            }
            _ => panic!("Expected struct type"),
        }
    }

    pub fn struct_field_type(&self, field_name: &str) -> IrType {
        match self {
            IrType::Struct { fields, .. } => fields
                .iter()
                .find(|f| f.name == field_name)
                .map(|f| f.ty.clone())
                .unwrap_or_else(|| panic!("Field not found in struct")),
            _ => panic!("Expected struct type"),
        }
    }

    pub fn has_field(&self, field_name: &str) -> bool {
        match self {
            IrType::Struct { fields, .. } => fields.iter().any(|f| f.name == field_name),
            _ => false,
        }
    }
}

// ----------- Function -----------

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub ret_ty: IrType,
    pub ret_temp: Option<IrTempId>,
    pub temps: Vec<IrTemp>,
    pub blocks: IndexMap<IrBlockId, IrBlock>,
    pub cfg: ControlFlowGraph,
}

impl IrFunction {
    pub fn temp_type(&self, id: IrTempId) -> &IrType {
        &self.temps[id.0 as usize].ty
    }
}

// ----------- Block -----------

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrBlockId(pub(crate) u32);

impl IrBlockId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct IrBlock {
    pub id: IrBlockId,
    pub name: String,
    pub insts: Vec<IrInst>,
    pub term: IrTerminator,
}

impl IrBlock {
    pub fn id(&self) -> IrBlockId {
        self.id
    }
    pub fn name(&self) -> &String {
        &self.name
    }
    pub fn insts(&self) -> &[IrInst] {
        &self.insts
    }
    pub fn term(&self) -> &IrTerminator {
        &self.term
    }
}

// ----------- Instruction -----------

#[derive(Debug, Clone)]
pub enum IrInst {
    Copy {
        dest: IrTempId,
        src: IrOperand,
    },
    BinaryOp {
        result: IrTempId,
        op: BinaryOp,
        lhs: IrOperand,
        rhs: IrOperand,
    },
    UnaryOp {
        result: IrTempId,
        op: UnaryOp,
        operand: IrOperand,
    },

    Call {
        result: Option<IrTempId>,
        name: String,
        args: Vec<IrOperand>,
        ret_ty: IrType,
    },

    Phi {
        result: IrTempId,
        incoming: Vec<(IrBlockId, IrTempId)>,
    },

    Store {
        base: IrTempId,
        byte_offset: IrOperand,
        value: IrOperand,
    },
    Load {
        base: IrTempId,
        byte_offset: IrOperand,
        result: IrTempId,
    },

    MemCopy {
        dest: IrTempId,
        src: IrTempId,
        dest_offset: IrOperand,
        src_offset: IrOperand,
        length: usize,
    },
}

impl IrInst {
    pub fn get_dest(&self) -> Option<IrTempId> {
        match self {
            IrInst::Copy { dest, .. } => Some(*dest),
            IrInst::BinaryOp { result, .. } => Some(*result),
            IrInst::UnaryOp { result, .. } => Some(*result),
            IrInst::Call { result, .. } => *result,
            IrInst::Phi { result, .. } => Some(*result),
            IrInst::Store { .. } => None,
            IrInst::Load { result, .. } => Some(*result),
            IrInst::MemCopy { dest, .. } => Some(*dest),
        }
    }

    pub fn get_sources(&self) -> Vec<IrOperand> {
        match self {
            IrInst::Copy { src, .. } => vec![*src],
            IrInst::BinaryOp { lhs, rhs, .. } => vec![*lhs, *rhs],
            IrInst::UnaryOp { operand, .. } => vec![*operand],
            IrInst::Call { args, .. } => args.clone(),
            IrInst::Phi { incoming, .. } => incoming
                .iter()
                .map(|(_, temp)| IrOperand::Temp(*temp))
                .collect(),
            IrInst::Store {
                base,
                byte_offset,
                value,
            } => {
                vec![IrOperand::Temp(*base), *byte_offset, *value]
            }
            IrInst::Load {
                base, byte_offset, ..
            } => {
                vec![IrOperand::Temp(*base), *byte_offset]
            }
            IrInst::MemCopy {
                dest,
                src,
                dest_offset,
                src_offset,
                ..
            } => {
                vec![
                    IrOperand::Temp(*dest),
                    IrOperand::Temp(*src),
                    *dest_offset,
                    *src_offset,
                ]
            }
        }
    }
}

// ----------- Terminator -----------

#[derive(Debug, Clone)]
pub enum IrTerminator {
    Ret {
        value: Option<IrOperand>,
    },
    Br {
        target: IrBlockId,
    },
    CondBr {
        cond: IrOperand,
        then_b: IrBlockId,
        else_b: IrBlockId,
    },
    #[doc(hidden)]
    _Unterminated,
}

// ----------- Display & Format -----------

impl fmt::Display for IrOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrOperand::Temp(temp) => write!(f, "%t{}", temp.id()),
            IrOperand::Const(c) => write!(f, "{c}"),
        }
    }
}

impl fmt::Display for IrTempId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%t{}", self.id())
    }
}

impl fmt::Display for IrTemp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.ty)?;
        match self.kind {
            IrTempKind::None => {}
            IrTempKind::Local => write!(f, " local")?,
            IrTempKind::Param { index } => write!(f, " param.{}", index)?,
            IrTempKind::Return => write!(f, " return")?,
        }
        if let Some(name) = &self.debug_name {
            write!(f, " (name: {})", name)?;
        }
        Ok(())
    }
}

impl fmt::Display for IrConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrConst::Int { value, .. } => write!(f, "{}", value),
            IrConst::Bool(value) => write!(f, "bool {}", value),
            IrConst::Unit => write!(f, "unit"),
        }
    }
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Unit => write!(f, "()"),
            IrType::Int { bits, signed } => {
                if *signed {
                    write!(f, "i{}", bits)
                } else {
                    write!(f, "u{}", bits)
                }
            }
            IrType::Bool => write!(f, "bool"),
            IrType::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
            }
            IrType::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "({})", fields_str.join(", "))
            }
            IrType::Struct { fields } => {
                let fields_str = fields.iter().map(|f| f.name.clone()).collect::<Vec<_>>();
                write!(f, "{{{}}}", fields_str.join(", "))
            }
        }
    }
}

impl fmt::Display for IrFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Function name
        write!(f, "fn {}(", self.name)?;

        // Parameters
        let mut param_pos = 0;
        for (i, temp) in self.temps.iter().enumerate() {
            if matches!(temp.kind, IrTempKind::Param { .. }) {
                if param_pos > 0 {
                    write!(f, ", ")?;
                }
                if let Some(name) = &temp.debug_name {
                    write!(f, "{name} [%t{}]: {}", i, temp.ty)?;
                } else {
                    write!(f, "%t{}: {}", i, temp.ty)?;
                }
                param_pos += 1;
            }
        }

        // Return type
        write!(f, ") -> ")?;
        if self.ret_temp.is_some() {
            write!(f, "[%t0] ")?;
        }
        writeln!(f, "{} {{", self.ret_ty)?;

        // Blocks
        for (id, block) in self.blocks.iter() {
            if id.id() > 0 {
                writeln!(f)?;
            }
            writeln!(f, "{}:{}:", id.id(), block.name)?;

            // Instructions
            for inst in block.insts.iter() {
                write!(f, "  ")?;
                format_inst(f, inst, self)?;
                writeln!(f)?;
            }

            // Terminator
            write!(f, "  ")?;
            format_terminator(f, &block.term, self)?;
            writeln!(f)?;
        }
        writeln!(f, "}}")?;
        writeln!(f, "---")?;
        writeln!(f, "Temps:")?;
        for (i, temp) in self.temps.iter().enumerate() {
            writeln!(f, "  %t{}: {}", i, temp)?;
        }
        writeln!(f, "---")?;
        writeln!(f, "CFG:")?;
        write!(f, "{}", self.cfg)?;
        Ok(())
    }
}

fn format_inst(f: &mut fmt::Formatter<'_>, inst: &IrInst, func: &IrFunction) -> fmt::Result {
    match inst {
        IrInst::Copy { dest, src } => {
            write!(f, "%t{} = copy {src}", dest.id())?;
        }
        IrInst::BinaryOp {
            result,
            op,
            lhs,
            rhs,
        } => {
            write!(
                f,
                "%t{} = binop.{} {lhs}, {rhs} : {}",
                result.id(),
                format_binary_op(op),
                func.temp_type(*result)
            )?;
        }
        IrInst::UnaryOp {
            result,
            op,
            operand,
        } => {
            write!(
                f,
                "%t{} = unop.{} {operand} : {}",
                result.id(),
                format_unary_op(op),
                func.temp_type(*result)
            )?;
        }
        IrInst::Call {
            result,
            name,
            args,
            ret_ty,
        } => {
            if let Some(result) = result {
                write!(f, "%t{} = ", result.id())?;
            }
            write!(f, "{name}(")?;
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{arg}")?;
            }
            write!(f, ") : {}", ret_ty)?;
        }
        IrInst::Phi { result, incoming } => {
            write!(f, "%t{} = phi [", result.id())?;
            for (i, (block_id, value)) in incoming.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                let block_name = func.blocks[block_id].name.as_str();
                write!(f, "({} -> %t{})", block_name, value.id())?;
            }
            write!(f, "]")?;
        }
        IrInst::Store {
            base,
            byte_offset,
            value,
        } => {
            let value_ty = match value {
                IrOperand::Temp(temp) => func.temp_type(*temp),
                IrOperand::Const(c) => &c.type_of(),
            };
            write!(
                f,
                "store %t{}[off={byte_offset}] = {value} : {}",
                base.id(),
                value_ty
            )?;
        }
        IrInst::Load {
            base,
            byte_offset,
            result,
        } => {
            write!(
                f,
                "%t{} = load %t{}[off={byte_offset}] : {}",
                result.id(),
                base.id(),
                func.temp_type(*result)
            )?;
        }
        IrInst::MemCopy {
            dest,
            src,
            dest_offset,
            src_offset,
            length,
        } => {
            write!(
                f,
                "memcpy %t{}[off={dest_offset}] <- %t{}[off={src_offset}] : len={length}",
                dest.id(),
                src.id(),
            )?;
        }
    }
    Ok(())
}

fn format_terminator(
    f: &mut fmt::Formatter<'_>,
    terminator: &IrTerminator,
    func: &IrFunction,
) -> fmt::Result {
    match terminator {
        IrTerminator::Ret { value } => match value {
            Some(value) => write!(f, "ret {value}")?,
            None => write!(f, "ret")?,
        },
        IrTerminator::Br { target } => {
            let name = func.blocks[target].name.as_str();
            write!(f, "br {}", name)?;
        }
        IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        } => {
            let then_name = func.blocks[then_b].name.as_str();
            let else_name = func.blocks[else_b].name.as_str();
            write!(f, "condbr {cond}, {then_name}, {else_name}")?;
        }
        IrTerminator::_Unterminated => {
            write!(f, "<unterminated>")?;
        }
    }
    Ok(())
}

fn format_binary_op(op: &BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "add",
        BinaryOp::Sub => "sub",
        BinaryOp::Mul => "mul",
        BinaryOp::Div => "udiv",
        BinaryOp::Eq => "eq",
        BinaryOp::Ne => "ne",
        BinaryOp::Lt => "lt",
        BinaryOp::Gt => "gt",
        BinaryOp::LtEq => "le",
        BinaryOp::GtEq => "ge",
    }
}

fn format_unary_op(op: &UnaryOp) -> &'static str {
    match op {
        UnaryOp::Neg => "neg",
    }
}
