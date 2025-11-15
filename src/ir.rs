use crate::ast::{BinaryOp, UnaryOp};
use crate::types::Type;
use std::fmt;

#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IrBlockId(u32);

/// SSA-like ephemeral value (not enforced yet)
#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IrTempId(u32);

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct IrTempType {
    pub typ: Type,
}

/// Stable stack slot pointer (aggregate later)
#[allow(unused)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct IrAddrId(u32);

#[allow(unused)]
#[derive(Debug, Clone)]
pub struct IrAddrType {
    pub typ: Type,
    pub align: usize,
    pub size: usize,
}

#[allow(unused)]
#[derive(Debug)]
pub struct IrFunction {
    pub name: String,
    pub params: Vec<IrParam>,
    pub return_type: Type,
    pub blocks: Vec<IrBlock>,
    pub temps: Vec<IrTempType>,
    pub addrs: Vec<IrAddrType>,
}

#[allow(unused)]
impl IrFunction {
    pub fn temp_type(&self, id: IrTempId) -> Type {
        self.temps[id.0 as usize].typ
    }

    pub fn addr_type(&self, id: IrAddrId) -> Type {
        self.addrs[id.0 as usize].typ
    }
}

#[derive(Debug)]
#[allow(unused)]
pub struct IrParam {
    pub name: String,
    pub typ: Type,
}

#[derive(Debug)]
#[allow(unused)]
pub struct IrBlock {
    id: IrBlockId,
    name: String,
    insts: Vec<IrInst>,
    term: IrTerminator,
}

#[allow(unused)]
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
#[allow(unused)]
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
#[allow(unused)]
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
}

// -----------------------------------------------------------------------------
// IR Builder
// -----------------------------------------------------------------------------

#[allow(unused)]
pub struct IrBlockBuilder<'a> {
    id: IrBlockId,
    name: String,
    insts: Vec<IrInst>,
    temps: &'a mut Vec<IrTempType>,
    addrs: &'a mut Vec<IrAddrType>,
}

#[allow(unused)]
impl<'a> IrBlockBuilder<'a> {
    pub fn new(
        id: IrBlockId,
        name: String,
        temps: &'a mut Vec<IrTempType>,
        addrs: &'a mut Vec<IrAddrType>,
    ) -> Self {
        Self {
            id,
            insts: vec![],
            name,
            temps,
            addrs,
        }
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

    pub fn alloc_var(&mut self, typ: Type, name_hint: String) -> IrAddrId {
        let addr = self.new_addr(typ);
        self.insts.push(IrInst::AllocVar {
            addr,
            typ,
            name_hint,
        });
        addr
    }

    pub fn store_var(&mut self, addr: IrAddrId, value: IrTempId) {
        let typ = self.temps[value.0 as usize].typ;
        debug_assert_eq!(self.addrs[addr.0 as usize].typ, typ);
        self.insts.push(IrInst::StoreVar { addr, value, typ });
    }

    pub fn load_var(&mut self, addr: IrAddrId) -> IrTempId {
        let typ = self.addrs[addr.0 as usize].typ;
        let value = self.new_temp(typ);
        self.insts.push(IrInst::LoadVar { value, addr, typ });
        value
    }

    pub fn const_u32(&mut self, result: IrTempId, value: u32) {
        self.insts.push(IrInst::ConstU32 { result, value });
    }

    pub fn const_bool(&mut self, result: IrTempId, value: bool) {
        self.insts.push(IrInst::ConstBool { result, value });
    }

    pub fn const_unit(&mut self, result: IrTempId) {
        self.insts.push(IrInst::ConstUnit { result });
    }

    pub fn binary_op(&mut self, result: IrTempId, op: BinaryOp, lhs: IrTempId, rhs: IrTempId) {
        let lhs_type = self.temps[lhs.0 as usize].typ;
        let rhs_type = self.temps[rhs.0 as usize].typ;
        debug_assert_eq!(lhs_type, rhs_type);

        let expected_type = Self::type_for_binary_op(op, lhs_type);
        let result_type = self.temps[result.0 as usize].typ;
        debug_assert_eq!(expected_type, result_type);

        self.insts.push(IrInst::BinaryOp {
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

        self.insts.push(IrInst::UnaryOp {
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
        self.insts.push(IrInst::Call {
            result,
            name,
            args,
            return_type,
        });
    }

    pub fn terminate(self, term: IrTerminator) -> IrBlock {
        // Finalize the block by consuming the builder, setting the terminator, and returning the
        // terminated block. This can be called only once per block builder since the
        // builder doesn't derive Clone/Copy.
        assert!(
            term != IrTerminator::_Unterminated,
            "Cannot terminate block with Unterminated"
        );
        IrBlock {
            id: self.id,
            name: self.name,
            insts: self.insts,
            term,
        }
    }
}

#[allow(unused)]
pub struct IrFunctionBuilder {
    name: String,
    params: Vec<IrParam>,
    return_type: Type,
    blocks: Vec<IrBlock>,
    temps: Vec<IrTempType>,
    addrs: Vec<IrAddrType>,
}

#[allow(unused)]
impl IrFunctionBuilder {
    pub fn new(name: String, params: Vec<IrParam>, return_type: Type) -> Self {
        Self {
            name,
            params,
            return_type,
            blocks: vec![],
            temps: vec![],
            addrs: vec![],
        }
    }

    pub fn new_temp(&mut self, typ: Type) -> IrTempId {
        let id = IrTempId(self.temps.len() as u32);
        self.temps.push(IrTempType { typ });
        id
    }

    pub fn new_block(&mut self, name: String) -> IrBlockId {
        let block_id = IrBlockId(self.blocks.len() as u32);
        let placeholder = IrBlock {
            id: block_id,
            name,
            insts: vec![],
            term: IrTerminator::_Unterminated,
        };
        self.blocks.push(placeholder);
        block_id
    }

    pub fn build_block<E, F>(&mut self, block_id: IrBlockId, f: F) -> Result<(), E>
    where
        F: FnOnce(IrBlockBuilder) -> Result<IrBlock, E>,
    {
        assert!(block_id.0 < self.blocks.len() as u32, "Block ID not found");
        let block_builder = IrBlockBuilder::new(
            block_id,
            self.blocks[block_id.0 as usize].name.clone(),
            &mut self.temps,
            &mut self.addrs,
        );
        let block = f(block_builder)?;
        self.blocks[block_id.0 as usize] = block; // replace the placeholder with the actual block
        Ok(())
    }

    pub fn build_expr_block<E, F>(
        &mut self,
        block_id: IrBlockId,
        result: IrTempId,
        f: F,
    ) -> Result<(), E>
    where
        F: FnOnce(IrBlockBuilder, IrTempId) -> Result<IrBlock, E>,
    {
        assert!(block_id.0 < self.blocks.len() as u32, "Block ID not found");
        let block_builder = IrBlockBuilder::new(
            block_id,
            self.blocks[block_id.0 as usize].name.clone(),
            &mut self.temps,
            &mut self.addrs,
        );
        let block = f(block_builder, result)?;
        self.blocks[block_id.0 as usize] = block; // replace the placeholder with the actual block
        Ok(())
    }

    pub fn finish(self) -> IrFunction {
        // Note: No need to check if blocks are terminated, since by design only terminated blocks
        // are added to the function in end_block.

        // Check that we have at least one block
        assert!(!self.blocks.is_empty(), "No blocks in function");

        // Check that all blocks are terminated
        for block in &self.blocks {
            assert!(
                block.term != IrTerminator::_Unterminated,
                "Block ({}) is not terminated",
                block.name
            );
        }

        let temps_vec = self.temps.clone();
        let addrs_vec = self.addrs.clone();
        IrFunction {
            name: self.name,
            params: self.params,
            return_type: self.return_type,
            blocks: self.blocks,
            temps: temps_vec,
            addrs: addrs_vec,
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
                addr.0, typ, addr_type.size, addr_type.align, name_hint
            )?;
        }
        IrInst::StoreVar { addr, value, .. } => {
            write!(
                f,
                "store %t{} -> &a{} : {}",
                addr.0,
                value.0,
                func.addr_type(*addr)
            )?;
        }
        IrInst::LoadVar { value, addr, .. } => {
            write!(
                f,
                "%t{} <- load &a{} : {}",
                value.0,
                addr.0,
                func.addr_type(*addr)
            )?;
        }
        IrInst::ConstU32 { result, value } => {
            write!(f, "%t{} = const.u32 {}", result.0, value)?;
        }
        IrInst::ConstBool { result, value } => {
            write!(f, "%t{} = const.bool {}", result.0, value)?;
        }
        IrInst::ConstUnit { result } => {
            write!(f, "%t{} = const.unit", result.0)?;
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
                result.0,
                format_binary_op(op),
                lhs.0,
                rhs.0,
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
                result.0,
                format_unary_op(op),
                operand.0,
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
                Some(result) => write!(f, "%t{} = {}(", result.0, name)?,
                None => write!(f, "{}(", name)?,
            }
            for (i, arg) in args.iter().enumerate() {
                if i > 0 {
                    write!(f, ", ")?;
                }
                write!(f, "%t{}", arg.0)?;
            }
            write!(f, ") : {}", return_type)?;
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
            Some(value) => write!(f, "ret %t{}", value.0)?,
            None => write!(f, "ret")?,
        },
        IrTerminator::Br { target } => {
            let name = func.blocks[target.0 as usize].name.as_str();
            write!(f, "br {}", name)?;
        }
        IrTerminator::CondBr {
            cond,
            then_b,
            else_b,
        } => {
            let then_name = func.blocks[then_b.0 as usize].name.as_str();
            let else_name = func.blocks[else_b.0 as usize].name.as_str();
            write!(f, "condbr %t{}, {}, {}", cond.0, then_name, else_name)?;
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
        let entry_bb = fn_builder.new_block("entry".to_string());
        let then_b = fn_builder.new_block("then".to_string());
        let else_b = fn_builder.new_block("else".to_string());
        let merge_b = fn_builder.new_block("merge".to_string());

        // Build the entry block
        fn_builder
            .build_block(entry_bb, |mut builder| -> Result<IrBlock, ()> {
                let lhs = builder.new_temp(Type::UInt32);
                let rhs = builder.new_temp(Type::UInt32);
                let cond = builder.new_temp(Type::Bool);
                builder.const_u32(lhs, 2);
                builder.const_u32(rhs, 1);
                builder.binary_op(cond, BinaryOp::Gt, lhs, rhs);
                Ok(builder.terminate(IrTerminator::CondBr {
                    cond,
                    then_b,
                    else_b,
                }))
            })
            .expect("Failed to build entry block");

        let if_result = fn_builder.new_temp(Type::UInt32);

        // Then block
        fn_builder
            .build_expr_block(
                then_b,
                if_result,
                |mut builder, result| -> Result<IrBlock, ()> {
                    builder.const_u32(result, 42);
                    Ok(builder.terminate(IrTerminator::Br { target: merge_b }))
                },
            )
            .expect("Failed to build then block");

        // Else block
        fn_builder
            .build_expr_block(
                else_b,
                if_result,
                |mut builder, result| -> Result<IrBlock, ()> {
                    builder.const_u32(result, 99);
                    Ok(builder.terminate(IrTerminator::Br { target: merge_b }))
                },
            )
            .expect("Failed to build else block");

        // Merge block
        fn_builder
            .build_block(merge_b, |builder| -> Result<IrBlock, ()> {
                Ok(builder.terminate(IrTerminator::Ret {
                    value: Some(if_result),
                }))
            })
            .expect("Failed to build merge block");

        let function = fn_builder.finish();

        assert_eq!(function.blocks.len(), 4);
        assert_eq!(function.blocks[0].insts.len(), 3);
        assert_eq!(function.blocks[1].insts.len(), 1);
        assert_eq!(function.blocks[2].insts.len(), 1);
        assert_eq!(function.blocks[3].insts.len(), 0);

        println!("{function}");

        // Output:
        // IrFunction {
        //     name: "foo",
        //     params: [],
        //     return_type: UInt32,
        //     blocks: [
        //         IrBlock {
        //             id: IrBlockId(0),
        //             name: "entry",
        //             insts: [
        //                 ConstU32 {
        //                     result: IrTempId(0),
        //                     value: 2
        //                 },
        //                 ConstU32 {
        //                     result: IrTempId(1),
        //                     value: 1
        //                 },
        //                 BinaryOp {
        //                     result: IrTempId(2),
        //                     op: Gt,
        //                     lhs: IrTempId(0),
        //                     rhs: IrTempId(1)
        //                 }
        //             ],
        //             term: CondBr {
        //                 cond: IrTempId(2),
        //                 then_b: IrBlockId(1),
        //                 else_b: IrBlockId(2)
        //             }
        //         },
        //         IrBlock {
        //             id: IrBlockId(1),
        //             name: "then",
        //             insts: [
        //                 ConstU32 {
        //                     result: IrTempId(3),
        //                     value: 42
        //                 }
        //             ],
        //             term: Br {
        //                 target: IrBlockId(3)
        //             }
        //         },
        //         IrBlock {
        //             id: IrBlockId(2),
        //             name: "else",
        //             insts: [
        //                 ConstU32 {
        //                     result: IrTempId(3),
        //                     value: 99
        //                 }
        //             ],
        //             term: Br {
        //                 target: IrBlockId(3)
        //             }
        //         },
        //         IrBlock {
        //             id: IrBlockId(3),
        //             name: "merge",
        //             insts: [],
        //             term: Ret {
        //                 value: Some(IrTempId(3))
        //             }
        //         }
        //     ],
        //     temps: [
        //         IrTempType {
        //             typ: UInt32
        //         },
        //         IrTempType {
        //             typ: UInt32
        //         },
        //         IrTempType {
        //             typ: Bool
        //         },
        //         IrTempType {
        //             typ: UInt32
        //         }
        //     ],
        //     addrs: []
        // }
    }
}
