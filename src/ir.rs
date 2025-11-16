use crate::ast::{BinaryOp, UnaryOp};
use crate::types::Type;
use std::fmt;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

#[derive(Debug)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: Type,
    pub blocks: Vec<IrBlock>,
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

#[derive(Debug)]
pub struct IrParam {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
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

#[derive(Debug, PartialEq, Eq)]
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

#[derive(Debug)]
pub enum IrInst {
    // Variable slots (scalars only for now)
    AllocVar {
        addr: IrAddrId,
        typ: Type,
        name_hint: String,
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
    blocks: Vec<IrBlock>,
    curr_block: IrBlockId,
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
            params,
            return_type,
            blocks: vec![entry_block],
            curr_block: IrBlockId(0),
            temps: vec![],
            addrs: vec![],
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
        assert!(
            self.blocks[idx].term == IrTerminator::_Unterminated,
            "Cannot select a terminated block: {}",
            self.blocks[idx].name
        );
        self.curr_block = id;
    }

    pub fn terminate(&mut self, term: IrTerminator) {
        assert!(
            term != IrTerminator::_Unterminated,
            "Cannot terminate block with Unterminated"
        );
        let block = self.curr_block_mut();
        assert!(
            block.term == IrTerminator::_Unterminated,
            "Block already terminated"
        );
        block.term = term;
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
            assert!(
                block.term != IrTerminator::_Unterminated,
                "Block '{}' is not terminated",
                block.name
            );
        }

        IrFunction {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.blocks,
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
        for (i, block) in self.blocks.iter().enumerate() {
            if i > 0 {
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
                let block_name = func.blocks[block_id.id() as usize].name.as_str();
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
            let name = func.blocks[target.id() as usize].name.as_str();
            write!(f, "br {}", name)?;
        }
        IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        } => {
            let then_name = func.blocks[then_b.id() as usize].name.as_str();
            let else_name = func.blocks[else_b.id() as usize].name.as_str();
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
        assert_eq!(function.blocks[0].insts.len(), 3);
        assert_eq!(function.blocks[1].insts.len(), 1);
        assert_eq!(function.blocks[2].insts.len(), 1);
        assert_eq!(function.blocks[3].insts.len(), 1);

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
