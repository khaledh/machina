use indexmap::IndexMap;
use std::fmt;

use crate::ast::{BinaryOp, UnaryOp};
use crate::cfg::ControlFlowGraph;

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

#[derive(Clone, Debug, PartialEq, Eq)]
#[allow(dead_code)]
pub enum IrType {
    Unit,
    Int { bits: u8, signed: bool },
    Bool,
    Ptr(Box<IrType>),
    // Agg, Array, etc.
    Array { elem_ty: Box<IrType>, len: usize },
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            IrType::Unit => write!(f, "unit"),
            IrType::Int { bits, signed } => {
                if *signed {
                    write!(f, "i{}", bits)
                } else {
                    write!(f, "u{}", bits)
                }
            }
            IrType::Bool => write!(f, "bool"),
            IrType::Ptr(ty) => write!(f, "ptr {}", ty),
            IrType::Array { elem_ty, len } => write!(f, "array[{}, {}]", elem_ty, len),
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
            IrType::Array { elem_ty, len } => (*elem_ty).size_of() * *len,
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
        }
    }

    pub fn is_compound(&self) -> bool {
        matches!(self, IrType::Array { .. })
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

    // Calls (all args passed by value; no aggregates yet)
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

    // Array element access
    StoreElement {
        array: IrTempId,
        index: IrOperand,
        value: IrOperand,
    },
    LoadElement {
        result: IrTempId,
        array: IrTempId,
        index: IrOperand,
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
            IrInst::StoreElement { .. } => None,
            IrInst::LoadElement { result, .. } => Some(*result),
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
            IrInst::StoreElement {
                array,
                index,
                value,
            } => {
                vec![IrOperand::Temp(*array), *index, *value]
            }
            IrInst::LoadElement { array, index, .. } => {
                vec![IrOperand::Temp(*array), *index]
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
                write!(f, "{}: {}", name, format_type(&temp.ty))?;
            } else {
                write!(f, "%t{}: {}", i, format_type(&temp.ty))?;
            }
        }

        // Return type
        writeln!(f, ") -> {} {{", format_type(&self.ret_ty))?;

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
        writeln!(f, "CFG:")?;
        write!(f, "{}", self.cfg)?;
        Ok(())
    }
}

fn format_type(ty: &IrType) -> String {
    match ty {
        IrType::Unit => "()".to_string(),
        IrType::Bool => "bool".to_string(),
        IrType::Int { bits, signed } => {
            if *signed {
                format!("i{}", bits)
            } else {
                format!("u{}", bits)
            }
        }
        IrType::Ptr(ty) => {
            format!("ptr {}", format_type(ty))
        }
        IrType::Array { elem_ty, len } => {
            format!("array[{}, {}]", format_type(elem_ty), len)
        }
    }
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
                format_type(func.temp_type(*result))
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
                format_type(func.temp_type(*result))
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
            write!(f, ") : {}", format_type(ret_ty))?;
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
        IrInst::StoreElement {
            array,
            index,
            value,
        } => {
            write!(
                f,
                "store.element %t{}, {}, {}",
                array.id(),
                format_operand(index),
                format_operand(value)
            )?;
        }
        IrInst::LoadElement {
            result,
            array,
            index,
        } => {
            write!(
                f,
                "%t{} = load.element %t{}, {}",
                result.id(),
                array.id(),
                format_operand(index)
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
