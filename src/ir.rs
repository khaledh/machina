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
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IrTempId(pub(crate) u32);

impl IrTempId {
    #[inline]
    pub fn id(&self) -> u32 {
        self.0
    }
}

/// Stable stack slot pointer (aggregate later)
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

// -----------------------------------------------------------------------------
// IR Builder
// -----------------------------------------------------------------------------

pub struct IrFunctionBuilder {
    name: String,
    params: Vec<IrParam>,
    return_type: Type,
    curr_block: IrBlockId,
    blocks: Vec<IrBlock>,
    termination_order: Vec<IrBlockId>,
    temps: Vec<IrTempType>,
    addrs: Vec<IrAddrType>,
}

impl IrFunctionBuilder {
    pub fn new(name: String, params: Vec<IrParam>, return_type: Type) -> Self {
        let entry_block = IrBlock {
            id: IrBlockId(0),
            name: "entry".to_string(),
            insts: vec![],
            term: IrTerminator::_Unterminated,
        };

        Self {
            name,
            params: params.clone(),
            return_type,
            blocks: vec![entry_block],
            curr_block: IrBlockId(0),
            temps: vec![],
            addrs: vec![],
            termination_order: vec![],
        }
    }

    pub fn new_block(&mut self, name: String) -> IrBlockId {
        let block_id = IrBlockId(self.blocks.len() as u32);
        self.blocks.push(IrBlock {
            id: block_id,
            name,
            insts: vec![],
            term: IrTerminator::_Unterminated,
        });
        block_id
    }

    #[inline]
    fn curr_block(&self) -> &IrBlock {
        &self.blocks[self.curr_block.id() as usize]
    }

    #[inline]
    fn curr_block_mut(&mut self) -> &mut IrBlock {
        let idx = self.curr_block.id() as usize;
        &mut self.blocks[idx]
    }

    pub fn select_block(&mut self, id: IrBlockId) {
        let idx = id.id() as usize;
        assert!(idx < self.blocks.len(), "Block ID not found: {}", id.id());
        assert_eq!(
            self.blocks[idx].term,
            IrTerminator::_Unterminated,
            "Cannot select a terminated block: {}",
            self.blocks[idx].name
        );
        self.curr_block = id;
    }

    pub fn terminate(&mut self, term: IrTerminator) {
        assert_ne!(
            term,
            IrTerminator::_Unterminated,
            "Cannot terminate block with Unterminated"
        );
        let block = self.curr_block_mut();
        assert_eq!(
            block.term,
            IrTerminator::_Unterminated,
            "Block already terminated"
        );
        block.term = term;
        // maintain termination order
        self.termination_order.push(self.curr_block);
    }

    pub fn new_addr(&mut self, typ: Type) -> IrAddrId {
        let id = IrAddrId(self.addrs.len() as u32);
        let size = typ.size_of();
        let align = typ.align_of();
        self.addrs.push(IrAddrType { typ, size, align });
        id
    }

    pub fn new_temp(&mut self, typ: Type) -> IrTempId {
        let id = IrTempId(self.temps.len() as u32);
        self.temps.push(IrTempType { typ });
        id
    }

    fn emit_inst(&mut self, inst: IrInst) {
        debug_assert!(
            matches!(self.curr_block().term, IrTerminator::_Unterminated),
            "Cannot emit into terminated block"
        );
        if let IrInst::Phi { .. } = inst {
            let block = self.curr_block();
            debug_assert!(
                block.insts.is_empty()
                    || block.insts.iter().all(|i| matches!(i, IrInst::Phi { .. })),
                "Phi must appear before non-phi instructions in a block"
            );
        }
        self.curr_block_mut().insts.push(inst);
    }

    pub fn alloc_var(&mut self, typ: Type, name_hint: String) -> IrAddrId {
        let addr = self.new_addr(typ);
        self.emit_inst(IrInst::AllocVar {
            addr,
            typ,
            name_hint,
        });
        addr
    }

    pub fn store_param(&mut self, addr: IrAddrId, index: u32, typ: Type) {
        debug_assert_eq!(self.addrs[addr.0 as usize].typ, typ);
        self.emit_inst(IrInst::StoreParam { addr, index, typ });
    }

    pub fn store_var(&mut self, addr: IrAddrId, value: IrTempId) {
        let typ = self.temps[value.0 as usize].typ;
        debug_assert_eq!(self.addrs[addr.0 as usize].typ, typ);
        self.emit_inst(IrInst::StoreVar { addr, value, typ });
    }

    pub fn load_var(&mut self, addr: IrAddrId) -> IrTempId {
        let typ = self.addrs[addr.0 as usize].typ;
        let value = self.new_temp(typ);
        self.emit_inst(IrInst::LoadVar { value, addr, typ });
        value
    }

    pub fn load_var_into(&mut self, target: IrTempId, addr: IrAddrId) {
        let typ = self.addrs[addr.0 as usize].typ;
        debug_assert_eq!(self.temps[target.0 as usize].typ, typ);
        self.emit_inst(IrInst::LoadVar {
            value: target,
            addr,
            typ,
        });
    }

    pub fn const_u32(&mut self, result: IrTempId, value: u32) {
        self.emit_inst(IrInst::ConstU32 { result, value });
    }

    pub fn const_bool(&mut self, result: IrTempId, value: bool) {
        self.emit_inst(IrInst::ConstBool { result, value });
    }

    pub fn const_unit(&mut self, result: IrTempId) {
        self.emit_inst(IrInst::ConstUnit { result });
    }

    pub fn binary_op(&mut self, result: IrTempId, op: BinaryOp, lhs: IrTempId, rhs: IrTempId) {
        let lhs_type = self.temps[lhs.0 as usize].typ;
        let rhs_type = self.temps[rhs.0 as usize].typ;
        debug_assert_eq!(lhs_type, rhs_type);

        let expected_type = Self::type_for_binary_op(op, lhs_type);
        let result_type = self.temps[result.0 as usize].typ;
        debug_assert_eq!(expected_type, result_type);

        self.emit_inst(IrInst::BinaryOp {
            result,
            op,
            lhs,
            rhs,
        });
    }

    pub fn unary_op(&mut self, result: IrTempId, op: UnaryOp, operand: IrTempId) {
        let operand_type = self.temps[operand.0 as usize].typ;

        let expected_type = Self::type_for_unary_op(op, operand_type);
        let result_type = self.temps[result.0 as usize].typ;
        debug_assert_eq!(expected_type, result_type);

        self.emit_inst(IrInst::UnaryOp {
            result,
            op,
            operand,
        });
    }

    fn type_for_binary_op(op: BinaryOp, lhs: Type) -> Type {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => lhs,
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => Type::Bool,
        }
    }

    fn type_for_unary_op(op: UnaryOp, operand: Type) -> Type {
        match op {
            UnaryOp::Neg => operand,
        }
    }

    pub fn call(
        &mut self,
        result: Option<IrTempId>,
        name: String,
        args: Vec<IrTempId>,
        return_type: Type,
    ) {
        self.emit_inst(IrInst::Call {
            result,
            name,
            args,
            return_type,
        });
    }

    pub fn phi(&mut self, result: IrTempId, incoming: Vec<(IrBlockId, IrTempId)>) {
        // assert that all incoming temps share the same type and match result’s type.
        let expected = self.temps[result.id() as usize].typ;
        debug_assert!(
            incoming
                .iter()
                .all(|(_, t)| self.temps[t.id() as usize].typ == expected)
        );
        self.emit_inst(IrInst::Phi { result, incoming });
    }

    pub fn finish(self) -> IrFunction {
        // Check that we have at least one block
        assert!(!self.blocks.is_empty(), "No blocks in function");

        // Check that all blocks are terminated
        for block in &self.blocks {
            assert_ne!(
                block.term,
                IrTerminator::_Unterminated,
                "Block '{}' is not terminated",
                block.name
            );
        }

        // Check that termination_order is complete and references blocks in the function (id < len)
        assert_eq!(self.termination_order.len(), self.blocks.len());
        assert!(
            self.termination_order
                .iter()
                .all(|id| id.id() < self.blocks.len() as u32)
        );

        // Build an IndexMap from block id to block in termination order
        let mut ordered_blocks = IndexMap::new();
        for id in self.termination_order.iter() {
            ordered_blocks.insert(*id, self.blocks[id.id() as usize].clone());
        }

        // Check that the last block is terminated with a ret
        let (_, last_block) = ordered_blocks.last().unwrap();
        assert!(
            matches!(last_block.term, IrTerminator::Ret { .. }),
            "Last block must be terminated with a ret"
        );

        IrFunction {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: ordered_blocks,
            temps: self.temps,
            addrs: self.addrs,
        }
    }
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
mod tests {
    use super::*;

    /// This is just a proof of concept to validate how the IR builder works.
    #[test]
    pub fn test_if_expr() {
        // fn foo() -> u32 {
        //     if 2 > 1 { 42 } else { 99 }
        // }
        let mut fn_builder = IrFunctionBuilder::new("foo".to_string(), vec![], Type::UInt32);

        // Create the required blocks
        let then_b = fn_builder.new_block("then".to_string());
        let else_b = fn_builder.new_block("else".to_string());
        let merge_b = fn_builder.new_block("merge".to_string());

        // Build the entry block
        let lhs = fn_builder.new_temp(Type::UInt32);
        let rhs = fn_builder.new_temp(Type::UInt32);
        let cond = fn_builder.new_temp(Type::Bool);
        fn_builder.const_u32(lhs, 2);
        fn_builder.const_u32(rhs, 1);
        fn_builder.binary_op(cond, BinaryOp::Gt, lhs, rhs);
        fn_builder.terminate(IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        });

        // Then block
        fn_builder.select_block(then_b);
        let then_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.const_u32(then_result, 42);
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Else block
        fn_builder.select_block(else_b);
        let else_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.const_u32(else_result, 99);
        fn_builder.terminate(IrTerminator::Br { target: merge_b });

        // Merge block
        fn_builder.select_block(merge_b);
        let merge_result = fn_builder.new_temp(Type::UInt32);
        fn_builder.phi(
            merge_result,
            vec![(then_b, then_result), (else_b, else_result)],
        );
        fn_builder.terminate(IrTerminator::Ret {
            value: Some(merge_result),
        });

        let function = fn_builder.finish();

        assert_eq!(function.blocks.len(), 4);
        assert_eq!(function.blocks[&IrBlockId(0)].insts.len(), 3);
        assert_eq!(function.blocks[&IrBlockId(1)].insts.len(), 1);
        assert_eq!(function.blocks[&IrBlockId(2)].insts.len(), 1);
        assert_eq!(function.blocks[&IrBlockId(3)].insts.len(), 1);

        println!("{function}");

        // Output:
        // fn foo() -> u32 {
        // entry:
        //     %t0 = const.u32 2
        //     %t1 = const.u32 1
        //     %t2 = binop.gt %t0, %t1 : bool
        //     condbr %t2, then, else
        //
        // then:
        //     %t3 = const.u32 42
        //     br merge
        //
        // else:
        //     %t4 = const.u32 99
        //     br merge
        //
        // merge:
        //     %t5 = phi [(then -> %t3), (else -> %t4)]
        //     ret %t5
        // }
    }
}
