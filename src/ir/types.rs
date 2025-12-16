use indexmap::IndexMap;
use std::fmt;

use crate::ast::{BinaryOp, UnaryOp};
use crate::ir::cfg::ControlFlowGraph;

/// IR for Machina
///
/// ## Concepts:
///
/// - Functions are the basic units of lowering.
/// - Blocks are the basic units of control flow within a function.
/// - Instructions are the basic units of computation within a block.
/// - Temps are register-sized units of data within a function.
/// - Addresses are the basic units of memory within a function.
///
/// ## Functions
///
/// Each function has a name, parameters, return type, blocks, temps, and addresses. Functions are
/// lowered from a function AST node using an IR function builder.
///
/// ## Blocks
///
/// Each block has an id, name, instructions, and a terminator. Block ids are assigned sequentially
/// starting from 0 within a function. The function builder uses a vector to store the blocks,
/// indexed by block id.
///
/// ## IR Function Builder
///
/// - Supports creation of blocks, temps, and addresses.
/// - Always starts with an entry block.
/// - Other blocks can be selected for emission of instructions at any time (although only one block
///   can be selected at a time).
/// - Blocks must be terminated with a terminator instruction (e.g. `br`, `condbr`, `ret`).
/// - Returns an immutable `IrFunction` when finished.
///
/// ## Instructions
///
/// Each instruction has its own representation in the IR. Depending on the instruction, it may have
/// operands, a result, and a type.
///
/// ## Terminators
///
/// Each block must be terminated with a terminator instruction. The terminator instruction is the
/// last instruction in the block. The terminator instruction determines the next block to execute,
/// or the end of the function if the block is the last block in the function.
///
/// ## Temps
///
/// Each temp has a type. Temps are assigned sequentially starting from 0 within a function. Temps
/// are used to represent the result of an instruction or the value of a variable.
/// The function builder uses a vector to store the temps, indexed by temp id.
///
/// ## Addresses
///
/// Each address has a type. The address type includes its size and alignment. The function builder
/// uses a vector to store the addresses, indexed by address id.
///
/// ## Types
///
/// The IR uses the `Type` enum to represent the type of a value. The type enum is defined in the
/// `types` module.
///
/// ## Load and Store Instructions
///
/// The IR supports load and store instructions for variables. The load instruction reads the value
/// from an address and stores it in a temp. The store instruction writes the value from a temp to
/// an address.
///
/// ## Constants
///
/// The IR supports constants for the following types: u64, bool, and unit.
///
/// ## Operations
///
/// The IR supports binary operations (e.g. add, sub, mul, div) and unary operations (e.g. neg).
///
/// ## Calls
///
/// The call instruction takes a function name, arguments, and a return type.
///
/// ## Phi Instructions
///
/// The IR supports phi instructions for the control flow of the function. The phi instruction is
/// used to merge the values of the incoming blocks into a single value.

// ----------- Function -----------

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub ret_ty: IrType,
    pub ret_temp: Option<IrTempId>,
    pub temps: Vec<IrTempInfo>,
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

// ----------- Operand -----------

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrOperand {
    Temp(IrTempId),
    Const(IrConst),
}

impl fmt::Display for IrOperand {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrOperand::Temp(temp) => write!(f, "%t{}", temp.id()),
            IrOperand::Const(c) => write!(f, "const.{}", c),
        }
    }
}

// ----------- Temp -----------

/// SSA-like ephemeral value (not enforced yet)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrTempId(pub(crate) u32);

impl IrTempId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

impl fmt::Display for IrTempId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "%t{}", self.id())
    }
}

#[derive(Debug, Clone)]
pub struct IrTempInfo {
    pub ty: IrType,
    pub role: IrTempRole,
    pub debug_name: Option<String>,
}

impl fmt::Display for IrTempInfo {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.role {
            IrTempRole::Local => write!(f, "local"),
            IrTempRole::Param { index } => write!(f, "param.{}", index),
            IrTempRole::Return => write!(f, "return"),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IrType {
    Unit,
    Int {
        bits: u8,
        signed: bool,
    },
    Bool,
    Ptr(Box<IrType>),
    // Agg, Array, etc.
    Array {
        elem_ty: Box<IrType>,
        dims: Vec<usize>,
    },
    Tuple {
        fields: Vec<IrType>,
    },
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
            IrType::Ptr(ty) => write!(f, "ptr {}", ty),
            IrType::Array { elem_ty, dims } => {
                let dims_str = dims.iter().map(|d| d.to_string()).collect::<Vec<_>>();
                write!(f, "{}[{}]", elem_ty, dims_str.join(", "))
            }
            IrType::Tuple { fields } => {
                let fields_str = fields.iter().map(|f| f.to_string()).collect::<Vec<_>>();
                write!(f, "({})", fields_str.join(", "))
            }
        }
    }
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
            IrType::Ptr(ty) => ty.size_of(),
            IrType::Array { elem_ty, dims } => {
                let total_elems: usize = dims.iter().product();
                total_elems * elem_ty.size_of()
            }
            IrType::Tuple { fields } => {
                let total_size: usize = fields.iter().map(|f| f.size_of()).sum();
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
            IrType::Ptr(ty) => ty.align_of(),
            IrType::Array { elem_ty, .. } => (*elem_ty).align_of(),
            IrType::Tuple { fields } => fields.iter().map(|f| f.align_of()).max().unwrap_or(1),
        }
    }

    pub fn is_compound(&self) -> bool {
        matches!(self, IrType::Array { .. } | IrType::Tuple { .. })
    }
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum IrTempRole {
    Local,
    Param { index: u32 },
    Return,
}

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

impl fmt::Display for IrConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrConst::Int { value, .. } => write!(f, "{}", value),
            IrConst::Bool(value) => write!(f, "const.bool {}", value),
            IrConst::Unit => write!(f, "const.unit"),
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

// ----------- Instruction -----------

#[derive(Debug, Clone)]
pub enum IrInst {
    Move {
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
            IrInst::Move { dest, .. } => Some(*dest),
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
            IrInst::Move { src, .. } => vec![*src],
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

// ----------- Display -----------

impl fmt::Display for IrFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Function name
        write!(f, "fn {}(", self.name)?;

        // Parameters
        for (i, temp) in self
            .temps
            .iter()
            .filter(|temp| matches!(temp.role, IrTempRole::Param { .. }))
            .enumerate()
        {
            if let Some(name) = &temp.debug_name {
                write!(f, "{}: {}", name, temp.ty)?;
            } else {
                write!(f, "%t{}: {}", i, temp.ty)?;
            }
        }

        // Return type
        writeln!(f, ") -> {} {{", self.ret_ty)?;

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
            writeln!(f, "  {}", format_temp(i as u32, temp))?;
        }
        writeln!(f, "---")?;
        writeln!(f, "CFG:")?;
        write!(f, "{}", self.cfg)?;
        Ok(())
    }
}

fn format_temp(id: u32, temp_info: &IrTempInfo) -> String {
    format!("%t{}: {} : {}", id, temp_info, temp_info.ty)
}

fn format_operand(operand: &IrOperand) -> String {
    match operand {
        IrOperand::Temp(temp) => format!("%t{}", temp.id()),
        IrOperand::Const(c) => format!("const.{}", c),
    }
}

fn format_inst(f: &mut fmt::Formatter<'_>, inst: &IrInst, func: &IrFunction) -> fmt::Result {
    match inst {
        IrInst::Move { dest, src } => {
            write!(f, "%t{} = move {}", dest.id(), format_operand(src))?;
        }
        IrInst::BinaryOp {
            result,
            op,
            lhs,
            rhs,
        } => {
            write!(
                f,
                "%t{} = binop.{} {}, {} : {}",
                result.id(),
                format_binary_op(op),
                format_operand(lhs),
                format_operand(rhs),
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
                "%t{} = unop.{} %t{} : {}",
                result.id(),
                format_unary_op(op),
                format_operand(operand),
                func.temp_type(*result)
            )?;
        }
        IrInst::Call {
            result,
            name,
            args,
            ret_ty,
        } => {
            match result {
                Some(result) => write!(f, "%t{} = {}(", result.id(), name)?,
                None => write!(f, "{}(", name)?,
            }
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "{}", format_operand(arg))?;
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
            write!(
                f,
                "store %t{}[offset={}] = {}",
                base.id(),
                format_operand(byte_offset),
                format_operand(value)
            )?;
        }
        IrInst::Load {
            base,
            byte_offset,
            result,
        } => {
            write!(
                f,
                "%t{} = load %t{}[offset={}] : {}",
                result.id(),
                base.id(),
                format_operand(byte_offset),
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
                "memcpy %t{}[offset={}] = %t{}[offset={}] : length={}",
                dest.id(),
                format_operand(dest_offset),
                src.id(),
                format_operand(src_offset),
                length
            )?;
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

fn format_terminator(
    f: &mut fmt::Formatter<'_>,
    terminator: &IrTerminator,
    func: &IrFunction,
) -> fmt::Result {
    match terminator {
        IrTerminator::Ret { value } => match value {
            Some(value) => write!(f, "ret {}", format_operand(value))?,
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
            write!(
                f,
                "condbr {}, {}, {}",
                format_operand(cond),
                then_name,
                else_name
            )?;
        }
        IrTerminator::_Unterminated => {
            write!(f, "<unterminated>")?;
        }
    }
    Ok(())
}
