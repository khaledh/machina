pub mod builder;

use indexmap::IndexMap;
use std::fmt;

use crate::ast::{BinaryOp, UnaryOp};
use crate::types::Type;

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
/// The IR supports constants for the following types: u32, bool, and unit.
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

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrBlockId(pub(crate) u32);

impl IrBlockId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct IrTempType {
    pub typ: Type,
}

/// SSA-like ephemeral value (not enforced yet)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrTempId(pub(crate) u32);

impl IrTempId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

/// Stable stack slot pointer (aggregate later)
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct IrAddrId(pub(crate) u32);

impl IrAddrId {
    pub fn id(&self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct IrAddrType {
    pub typ: Type,
    pub align: usize,
    pub size: usize,
}

#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: Type,
    pub blocks: IndexMap<IrBlockId, IrBlock>,
    pub temps: Vec<IrTempType>,
    pub addrs: Vec<IrAddrType>,
}

impl IrFunction {
    pub fn temp_type(&self, id: IrTempId) -> Type {
        self.temps[id.0 as usize].typ
    }

    pub fn addr_type(&self, id: IrAddrId) -> Type {
        self.addrs[id.0 as usize].typ
    }
}

#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug, Clone)]
pub struct IrBlock {
    id: IrBlockId,
    name: String,
    insts: Vec<IrInst>,
    term: IrTerminator,
}

impl IrBlock {
    pub fn id(&self) -> IrBlockId {
        self.id
    }
    pub fn name(&self) -> &String {
        &self.name
    }
    pub fn insts(&self) -> &Vec<IrInst> {
        &self.insts
    }
    pub fn term(&self) -> &IrTerminator {
        &self.term
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum IrTerminator {
    Ret {
        value: Option<IrTempId>,
    },
    Br {
        target: IrBlockId,
    },
    CondBr {
        cond: IrTempId,
        then_b: IrBlockId,
        else_b: IrBlockId,
    },
    #[doc(hidden)]
    _Unterminated,
}

#[derive(Debug, Clone)]
pub enum IrInst {
    // Variable slots (scalars only for now)
    AllocVar {
        addr: IrAddrId,
        typ: Type,
        name_hint: String,
    },
    StoreParam {
        addr: IrAddrId,
        index: u32,
        typ: Type,
    },
    StoreVar {
        addr: IrAddrId,
        value: IrTempId,
        typ: Type,
    },
    LoadVar {
        value: IrTempId,
        addr: IrAddrId,
        typ: Type,
    },

    // Constants
    ConstU32 {
        result: IrTempId,
        value: u32,
    },
    ConstBool {
        result: IrTempId,
        value: bool,
    },
    ConstUnit {
        result: IrTempId,
    },

    // Operations
    BinaryOp {
        result: IrTempId,
        op: BinaryOp,
        lhs: IrTempId,
        rhs: IrTempId,
    },
    UnaryOp {
        result: IrTempId,
        op: UnaryOp,
        operand: IrTempId,
    },

    // Calls (all args passed by value; no aggregates yet)
    Call {
        result: Option<IrTempId>,
        name: String,
        args: Vec<IrTempId>,
        return_type: Type,
    },

    Phi {
        result: IrTempId,
        incoming: Vec<(IrBlockId, IrTempId)>,
    },
}

impl fmt::Display for IrFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Function name
        write!(f, "fn {}(", self.name)?;

        // Parameters
        for (i, param) in self.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: {}", param.name, param.typ)?;
        }

        // Return type
        writeln!(f, ") -> {} {{", self.return_type)?;

        // Blocks
        for (id, block) in self.blocks.iter() {
            if id.id() > 0 {
                writeln!(f, "")?;
            }
            writeln!(f, "{}:", block.name)?;

            // Instructions
            for inst in block.insts.iter() {
                write!(f, "  ")?;
                format_inst(f, inst, self)?;
                writeln!(f, "")?;
            }

            // Terminator
            write!(f, "  ")?;
            format_terminator(f, &block.term, self)?;
            writeln!(f, "")?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

fn format_inst(f: &mut fmt::Formatter<'_>, inst: &IrInst, func: &IrFunction) -> fmt::Result {
    match inst {
        IrInst::StoreParam { addr, index, typ } => {
            write!(f, "store param.{} -> &a{} : {}", index, addr.id(), typ)?;
        }
        IrInst::AllocVar {
            addr,
            typ,
            name_hint,
        } => {
            let addr_type = &func.addrs[addr.0 as usize];
            write!(
                f,
                "&a{} = alloc {} (size={}, align={}) name_hint={}",
                addr.id(),
                typ,
                addr_type.size,
                addr_type.align,
                name_hint
            )?;
        }
        IrInst::StoreVar { addr, value, .. } => {
            write!(
                f,
                "store %t{} -> &a{} : {}",
                value.id(),
                addr.id(),
                func.addr_type(*addr)
            )?;
        }
        IrInst::LoadVar { value, addr, .. } => {
            write!(
                f,
                "%t{} <- load &a{} : {}",
                value.id(),
                addr.id(),
                func.addr_type(*addr)
            )?;
        }
        IrInst::ConstU32 { result, value } => {
            write!(f, "%t{} = const.u32 {}", result.id(), value)?;
        }
        IrInst::ConstBool { result, value } => {
            write!(f, "%t{} = const.bool {}", result.id(), value)?;
        }
        IrInst::ConstUnit { result } => {
            write!(f, "%t{} = const.unit", result.id())?;
        }
        IrInst::BinaryOp {
            result,
            op,
            lhs,
            rhs,
        } => {
            write!(
                f,
                "%t{} = binop.{} %t{}, %t{} : {}",
                result.id(),
                format_binary_op(op),
                lhs.id(),
                rhs.id(),
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
                operand.id(),
                func.temp_type(*result)
            )?;
        }
        IrInst::Call {
            result,
            name,
            args,
            return_type,
        } => {
            match result {
                Some(result) => write!(f, "%t{} = {}(", result.id(), name)?,
                None => write!(f, "{}(", name)?,
            }
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "%t{}", arg.id())?;
            }
            write!(f, ") : {}", return_type)?;
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
            Some(value) => write!(f, "ret %t{}", value.id())?,
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
            write!(f, "condbr %t{}, {}, {}", cond.id(), then_name, else_name)?;
        }
        IrTerminator::_Unterminated => {
            write!(f, "<unterminated>")?;
        }
    }
    Ok(())
}

#[cfg(test)]
#[path = "../tests/t_ir_builder.rs"]
mod tests;
