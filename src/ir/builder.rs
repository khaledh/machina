use indexmap::IndexMap;

use crate::ast::{BinaryOp, UnaryOp};
use crate::cfg::ControlFlowGraph;
use crate::ir::types::*;

pub struct IrFunctionBuilder {
    name: String,
    ret_ty: IrType,
    curr_block: IrBlockId,
    blocks: IndexMap<IrBlockId, IrBlock>,
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
            blocks: IndexMap::from_iter([(IrBlockId(0), entry_block)]),
            curr_block: IrBlockId(0),
            temps: vec![],
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
        self.blocks.insert(
            block_id,
            IrBlock {
                id: block_id,
                name,
                insts: vec![],
                term: IrTerminator::_Unterminated,
            },
        );
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

    pub fn finish(mut self) -> IrFunction {
        // Check that we have at least one block
        assert!(!self.blocks.is_empty(), "No blocks in function");

        // All blocks must be terminated
        for (_, block) in &self.blocks {
            assert!(
                !matches!(block.term, IrTerminator::_Unterminated),
                "Block '{}' is not terminated",
                block.name
            );
        }

        // Build the control flow graph
        let blocks_vec: Vec<IrBlock> = self.blocks.values().cloned().collect();
        let cfg = ControlFlowGraph::from(&blocks_vec);

        // Sort the blocks by their id in cfg.block_ids (sorted in RPO)
        let mut sorted_blocks = IndexMap::new();
        for block_id in &cfg.block_ids {
            let block = self
                .blocks
                .get(block_id)
                .expect("Block in CFG not found in blocks")
                .clone();
            sorted_blocks.insert(*block_id, block);
        }

        // Check that the function has at least one ret terminator
        let has_ret = sorted_blocks
            .values()
            .any(|block| matches!(block.term, IrTerminator::Ret { .. }));
        assert!(
            has_ret,
            "Function must have at least one block with a ret terminator"
        );

        IrFunction {
            name: self.name,
            ret_ty: self.ret_ty,
            blocks: sorted_blocks,
            temps: self.temps,
            cfg,
        }
    }
}

#[cfg(test)]
#[path = "../tests/t_ir_builder.rs"]
mod tests;
