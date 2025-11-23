use indexmap::IndexMap;

use crate::ast::{BinaryOp, UnaryOp};
use crate::cfg::ControlFlowGraph;
use crate::ir::types::*;

pub struct IrFunctionBuilder {
    name: String,
    ret_ty: IrType,
    curr_block: IrBlockId,
    blocks: Vec<IrBlock>,
    termination_order: Vec<IrBlockId>,
    temps: Vec<IrTempInfo>,
}

impl IrFunctionBuilder {
    pub fn new(name: String, ret_ty: IrType) -> Self {
        let entry_block = IrBlock {
            id: IrBlockId(0),
            name: "entry".to_string(),
            insts: vec![],
            term: IrTerminator::_Unterminated,
        };

        Self {
            name,
            ret_ty,
            blocks: vec![entry_block],
            curr_block: IrBlockId(0),
            temps: vec![],
            termination_order: vec![],
        }
    }

    pub fn new_param(&mut self, index: u32, name: String, ty: IrType) -> IrTempId {
        let id = IrTempId(self.temps.len() as u32);
        self.temps.push(IrTempInfo {
            ty,
            role: IrTempRole::Param { index },
            debug_name: Some(name),
        });
        id
    }

    pub fn to_local(&mut self, temp: IrTempId, name: String) {
        match self.temps.get_mut(temp.id() as usize) {
            Some(temp) => {
                temp.role = IrTempRole::Local;
                temp.debug_name = Some(name);
            }
            None => panic!("Temp not found: {}", temp.id()),
        }
    }

    pub fn new_temp(&mut self, ty: IrType) -> IrTempId {
        let id = IrTempId(self.temps.len() as u32);
        self.temps.push(IrTempInfo {
            ty,
            role: IrTempRole::Local,
            debug_name: None,
        });
        id
    }

    pub fn new_const_int(&mut self, value: i64, bits: u8, signed: bool) -> IrOperand {
        IrOperand::Const(IrConst::Int {
            value,
            bits,
            signed,
        })
    }

    pub fn new_const_bool(&mut self, value: bool) -> IrOperand {
        IrOperand::Const(IrConst::Bool(value))
    }

    pub fn new_const_unit(&mut self) -> IrOperand {
        IrOperand::Const(IrConst::Unit)
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
            matches!(self.blocks[idx].term, IrTerminator::_Unterminated),
            "Cannot select a terminated block: {}",
            self.blocks[idx].name
        );
        self.curr_block = id;
    }

    pub fn terminate(&mut self, term: IrTerminator) {
        assert!(
            !matches!(term, IrTerminator::_Unterminated),
            "Cannot terminate block with _Unterminated"
        );
        let block = self.curr_block_mut();
        assert!(
            matches!(block.term, IrTerminator::_Unterminated),
            "Block already terminated"
        );
        block.term = term;
        // maintain termination order
        self.termination_order.push(self.curr_block);
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

    pub fn move_to(&mut self, dest: IrTempId, src: IrOperand) {
        self.emit_inst(IrInst::Move { dest, src });
    }

    pub fn binary_op(&mut self, result: IrTempId, op: BinaryOp, lhs: IrOperand, rhs: IrOperand) {
        // TODO: assert that the types of the operands match the expected type for the binary operation.
        // let lhs_type = self.temps[lhs.0 as usize].ty;
        // let rhs_type = self.temps[rhs.0 as usize].ty;
        // debug_assert_eq!(lhs_type, rhs_type);

        // let expected_type = Self::type_for_binary_op(op, lhs_type);
        // let result_type = self.temps[result.0 as usize].ty;
        // debug_assert_eq!(expected_type, result_type);

        self.emit_inst(IrInst::BinaryOp {
            result,
            op,
            lhs,
            rhs,
        });
    }

    pub fn unary_op(&mut self, result: IrTempId, op: UnaryOp, operand: IrOperand) {
        // TODO: assert that the type of the operand matches the expected type for the unary operation.
        // let operand_type = self.temps[operand.0 as usize].ty;
        // let expected_type = Self::type_for_unary_op(op, operand_type);
        // let result_type = self.temps[result.0 as usize].ty;
        // debug_assert_eq!(expected_type, result_type);

        self.emit_inst(IrInst::UnaryOp {
            result,
            op,
            operand,
        });
    }

    fn type_for_binary_op(op: BinaryOp, lhs: IrType) -> IrType {
        match op {
            BinaryOp::Add | BinaryOp::Sub | BinaryOp::Mul | BinaryOp::Div => lhs,
            BinaryOp::Eq
            | BinaryOp::Ne
            | BinaryOp::Lt
            | BinaryOp::Gt
            | BinaryOp::LtEq
            | BinaryOp::GtEq => IrType::Bool,
        }
    }

    fn type_for_unary_op(op: UnaryOp, operand: IrType) -> IrType {
        match op {
            UnaryOp::Neg => operand,
        }
    }

    pub fn call(
        &mut self,
        result: Option<IrTempId>,
        name: String,
        args: Vec<IrOperand>,
        ret_ty: IrType,
    ) {
        self.emit_inst(IrInst::Call {
            result,
            name,
            args,
            ret_ty,
        });
    }

    pub fn phi(&mut self, result: IrTempId, incoming: Vec<(IrBlockId, IrOperand)>) {
        // TODO: assert that all incoming temps share the same type and match result’s type.
        // let expected = self.temps[result.id() as usize].ty;
        // debug_assert!(
        //     incoming
        //         .iter()
        //         .all(|(_, o)| o.get_type() == expected)
        // );
        self.emit_inst(IrInst::Phi { result, incoming });
    }

    pub fn finish(self) -> IrFunction {
        // Check that we have at least one block
        assert!(!self.blocks.is_empty(), "No blocks in function");

        // All blocks must be terminated
        for block in &self.blocks {
            assert!(
                !matches!(block.term, IrTerminator::_Unterminated),
                "Block '{}' is not terminated",
                block.name
            );
        }

        // termination_order must reference valid blocks and cover all of them exactly once
        assert_eq!(
            self.termination_order.len(),
            self.blocks.len(),
            "termination_order must contain each block exactly once"
        );
        let mut seen = vec![false; self.blocks.len()];
        for id in &self.termination_order {
            let idx = id.id() as usize;
            assert!(
                idx < self.blocks.len(),
                "termination_order contains invalid block id {}",
                id.id()
            );
            assert!(
                !seen[idx],
                "Block id {} appears multiple times in termination_order",
                id.id()
            );
            seen[idx] = true;
        }

        // Build an IndexMap from block id to block in termination order
        let mut ordered_blocks = IndexMap::new();
        for id in &self.termination_order {
            ordered_blocks.insert(*id, self.blocks[id.id() as usize].clone());
        }

        // Check that the last block is terminated with a ret
        let (_, last_block) = ordered_blocks.last().unwrap();
        assert!(
            matches!(last_block.term, IrTerminator::Ret { .. }),
            "Last block must be terminated with a ret"
        );

        // Build the control flow graph
        let cfg = ControlFlowGraph::from(&ordered_blocks);

        IrFunction {
            name: self.name,
            ret_ty: self.ret_ty,
            blocks: ordered_blocks,
            temps: self.temps,
            cfg,
        }
    }
}

#[cfg(test)]
#[path = "../tests/t_ir_builder.rs"]
mod tests;
