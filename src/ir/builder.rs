use indexmap::IndexMap;

use crate::ast::{BinaryOp, UnaryOp};
use crate::ir::types::*;
use crate::types::Type;

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

#[cfg(test)]
#[path = "../tests/t_ir_builder.rs"]
mod tests;
