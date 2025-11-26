use std::collections::HashSet;

use crate::ast::BinaryOp;
use crate::ir::builder::IrFunctionBuilder;
use crate::ir::types::{IrConst, IrOperand, IrTempId, IrTerminator, IrType};

// Helper to build a minimal function with given name / signature
fn mk_builder() -> IrFunctionBuilder {
    IrFunctionBuilder::new(
        "test".to_string(),
        IrType::Int {
            bits: 32,
            signed: false,
        },
    )
}

// Helpers for common types and operands

fn u32_ty() -> IrType {
    IrType::Int {
        bits: 32,
        signed: false,
    }
}

fn bool_ty() -> IrType {
    IrType::Bool
}

fn const_u32(value: i64) -> IrOperand {
    IrOperand::Const(IrConst::Int {
        value,
        bits: 32,
        signed: false,
    })
}

fn temp(id: u32) -> IrOperand {
    IrOperand::Temp(IrTempId(id))
}

#[test]
fn simple_linear_cfg() {
    // Build:
    //
    // entry:
    //   %t0 = move const.u32 1
    //   ret %t0
    //
    // Expected CFG:
    // - block_ids: [entry]
    // - succ: entry -> []
    // - pred: entry -> []
    let mut b = mk_builder();

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));
    b.terminate(IrTerminator::Ret {
        value: Some(temp(0)),
    });

    let func = b.finish();
    let cfg = &func.cfg;

    // Should have exactly one block
    assert_eq!(cfg.block_ids.len(), 1);

    let entry_id = func.blocks.keys().next().unwrap();

    // Entry should have no successors or predecessors
    assert!(cfg.succ[entry_id].is_empty());
    assert!(cfg.pred[entry_id].is_empty());
}

#[test]
fn cfg_with_unconditional_branch() {
    // Build:
    //
    // entry:
    //   %t0 = move const.u32 1
    //   br then
    //
    // then:
    //   ret %t0
    //
    // Expected CFG:
    // - block_ids: [entry, then]
    // - succ: entry -> [then], then -> []
    // - pred: entry -> [], then -> [entry]
    let mut b = mk_builder();

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));

    let then_id = b.new_block("then".to_string());
    b.terminate(IrTerminator::Br { target: then_id });

    b.select_block(then_id);
    b.terminate(IrTerminator::Ret {
        value: Some(temp(0)),
    });

    let func = b.finish();
    let cfg = &func.cfg;

    assert_eq!(cfg.block_ids.len(), 2);

    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let then_id = blocks_iter.next().unwrap();

    // Entry should have then as successor, no predecessors
    assert_eq!(cfg.succ[entry_id], vec![*then_id]);
    assert!(cfg.pred[entry_id].is_empty());

    // Then should have no successors, entry as predecessor
    assert!(cfg.succ[then_id].is_empty());
    assert_eq!(cfg.pred[then_id], vec![*entry_id]);
}

#[test]
fn cfg_with_conditional_branch() {
    // Build:
    //
    // entry:
    //   %t0 = move const.u32 1
    //   %t1 = binop.lt %t0, const.u32 2
    //   condbr %t1, then, else
    //
    // then:
    //   ret const.u32 10
    //
    // else:
    //   ret const.u32 20
    //
    // Expected CFG:
    // - block_ids: [entry, then, else]
    // - succ: entry -> [then, else], then -> [], else -> []
    // - pred: entry -> [], then -> [entry], else -> [entry]
    let mut b = mk_builder();

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));
    let t1 = b.new_temp(bool_ty());
    b.binary_op(t1, BinaryOp::Lt, temp(0), const_u32(2));
    b.terminate(IrTerminator::CondBr {
        cond: temp(1),
        then_b: then_id,
        else_b: else_id,
    });

    b.select_block(then_id);
    b.terminate(IrTerminator::Ret {
        value: Some(const_u32(10)),
    });

    b.select_block(else_id);
    b.terminate(IrTerminator::Ret {
        value: Some(const_u32(20)),
    });

    let func = b.finish();
    let cfg = &func.cfg;

    assert_eq!(cfg.block_ids.len(), 3);

    let entry_id = func.blocks.keys().next().unwrap();

    // Entry should have then and else as successors (in that order for stable RPO)
    assert_eq!(cfg.succ[entry_id], vec![then_id, else_id]);
    assert!(cfg.pred[entry_id].is_empty());

    // Then should have no successors, entry as predecessor
    assert!(cfg.succ[&then_id].is_empty());
    assert_eq!(cfg.pred[&then_id], vec![*entry_id]);

    // Else should have no successors, entry as predecessor
    assert!(cfg.succ[&else_id].is_empty());
    assert_eq!(cfg.pred[&else_id], vec![*entry_id]);
}

#[test]
fn cfg_with_join_block() {
    // Build:
    //
    // entry:
    //   %t0 = move const.u32 1
    //   %t1 = binop.lt %t0, const.u32 2
    //   condbr %t1, then, else
    //
    // then:
    //   br join
    //
    // else:
    //   br join
    //
    // join:
    //   ret const.u32 0
    //
    // Expected CFG:
    // - block_ids: [entry, then, else, join]
    // - succ: entry -> [then, else], then -> [join], else -> [join], join -> []
    // - pred: entry -> [], then -> [entry], else -> [entry], join -> [then, else]
    let mut b = mk_builder();

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());
    let join_id = b.new_block("join".to_string());

    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(1));
    let t1 = b.new_temp(bool_ty());
    b.binary_op(t1, BinaryOp::Lt, temp(0), const_u32(2));
    b.terminate(IrTerminator::CondBr {
        cond: temp(1),
        then_b: then_id,
        else_b: else_id,
    });

    b.select_block(then_id);
    b.terminate(IrTerminator::Br { target: join_id });

    b.select_block(else_id);
    b.terminate(IrTerminator::Br { target: join_id });

    b.select_block(join_id);
    b.terminate(IrTerminator::Ret {
        value: Some(const_u32(0)),
    });

    let func = b.finish();
    let cfg = &func.cfg;

    assert_eq!(cfg.block_ids.len(), 4);

    let entry_id = func.blocks.keys().next().unwrap();

    // Entry should have then and else as successors (in that order for stable RPO)
    assert_eq!(cfg.succ[entry_id], vec![then_id, else_id]);
    assert!(cfg.pred[entry_id].is_empty());

    // Then should have join as successor, entry as predecessor
    assert_eq!(cfg.succ[&then_id], vec![join_id]);
    assert_eq!(cfg.pred[&then_id], vec![*entry_id]);

    // Else should have join as successor, entry as predecessor
    assert_eq!(cfg.succ[&else_id], vec![join_id]);
    assert_eq!(cfg.pred[&else_id], vec![*entry_id]);

    // Join should have no successors, then and else as predecessors
    assert!(cfg.succ[&join_id].is_empty());
    assert_eq!(
        cfg.pred[&join_id].iter().cloned().collect::<HashSet<_>>(),
        HashSet::from([then_id, else_id])
    );
}

#[test]
fn cfg_rpo_ordering() {
    // Build a diamond-shaped CFG:
    //
    // entry:
    //   condbr cond, then, else
    //
    // then:
    //   br join
    //
    // else:
    //   br join
    //
    // join:
    //   ret
    //
    // RPO should be: [entry, then, else, join] or [entry, else, then, join]
    // Both are valid RPO orderings for this graph
    let mut b = mk_builder();

    let then_id = b.new_block("then".to_string());
    let else_id = b.new_block("else".to_string());
    let join_id = b.new_block("join".to_string());

    let t0 = b.new_temp(bool_ty());
    b.move_to(t0, IrOperand::Const(IrConst::Bool(true)));
    b.terminate(IrTerminator::CondBr {
        cond: temp(0),
        then_b: then_id,
        else_b: else_id,
    });

    b.select_block(then_id);
    b.terminate(IrTerminator::Br { target: join_id });

    b.select_block(else_id);
    b.terminate(IrTerminator::Br { target: join_id });

    b.select_block(join_id);
    b.terminate(IrTerminator::Ret { value: None });

    let func = b.finish();
    let cfg = &func.cfg;

    // Should have 4 blocks in RPO
    assert_eq!(cfg.block_ids.len(), 4);

    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let join_id = blocks_iter.skip(2).next().unwrap();

    // Entry should be first in RPO
    assert_eq!(cfg.block_ids[0], *entry_id);

    // Join should be last in RPO (since it's the sink node)
    assert_eq!(cfg.block_ids[3], *join_id);
}

#[test]
fn cfg_with_loop() {
    // Build a simple loop:
    //
    // entry:
    //   br loop_header
    //
    // loop_header:
    //   %t0 = move const.u32 0
    //   %t1 = binop.lt %t0, const.u32 10
    //   condbr %t1, loop_body, exit
    //
    // loop_body:
    //   br loop_header
    //
    // exit:
    //   ret
    //
    // Expected CFG:
    // - succ: entry -> [loop_header], loop_header -> [loop_body, exit],
    //         loop_body -> [loop_header], exit -> []
    // - pred: entry -> [], loop_header -> [entry, loop_body],
    //         loop_body -> [loop_header], exit -> [loop_header]
    let mut b = mk_builder();

    let loop_header_id = b.new_block("loop_header".to_string());
    let loop_body_id = b.new_block("loop_body".to_string());
    let exit_id = b.new_block("exit".to_string());

    // Entry block
    b.terminate(IrTerminator::Br {
        target: loop_header_id,
    });

    // Loop header
    b.select_block(loop_header_id);
    let t0 = b.new_temp(u32_ty());
    b.move_to(t0, const_u32(0));
    let t1 = b.new_temp(bool_ty());
    b.binary_op(t1, BinaryOp::Lt, temp(0), const_u32(10));
    b.terminate(IrTerminator::CondBr {
        cond: temp(1),
        then_b: loop_body_id,
        else_b: exit_id,
    });

    // Loop body
    b.select_block(loop_body_id);
    b.terminate(IrTerminator::Br {
        target: loop_header_id,
    });

    // Exit block
    b.select_block(exit_id);
    b.terminate(IrTerminator::Ret { value: None });

    let func = b.finish();
    let cfg = &func.cfg;

    assert_eq!(cfg.block_ids.len(), 4);

    let entry_id = func.blocks.keys().next().unwrap();

    // Entry should have loop_header as successor
    assert_eq!(cfg.succ[entry_id], vec![loop_header_id]);
    assert!(cfg.pred[entry_id].is_empty());

    // Loop header should have loop_body and exit as successors (in that order for stable RPO)
    // and entry and loop_body as predecessors (back edge)
    assert_eq!(cfg.succ[&loop_header_id], vec![loop_body_id, exit_id]);
    assert_eq!(
        cfg.pred[&loop_header_id]
            .iter()
            .cloned()
            .collect::<HashSet<_>>(),
        HashSet::from([*entry_id, loop_body_id])
    );

    // Loop body should have loop_header as successor (back edge)
    assert_eq!(cfg.succ[&loop_body_id], vec![loop_header_id]);
    assert_eq!(cfg.pred[&loop_body_id], vec![loop_header_id]);

    // Exit should have no successors, loop_header as predecessor
    assert!(cfg.succ[&exit_id].is_empty());
    assert_eq!(cfg.pred[&exit_id], vec![loop_header_id]);
}

#[test]
fn cfg_rpo_with_loop() {
    // Build a loop and verify RPO ordering places loop blocks correctly
    //
    // entry:
    //   br loop_header
    //
    // loop_header:
    //   condbr cond, loop_body, exit
    //
    // loop_body:
    //   br loop_header
    //
    // exit:
    //   ret
    //
    // In RPO, entry should come first, and exit should come after the loop
    let mut b = mk_builder();

    let loop_header_id = b.new_block("loop_header".to_string());
    let loop_body_id = b.new_block("loop_body".to_string());
    let exit_id = b.new_block("exit".to_string());

    b.terminate(IrTerminator::Br {
        target: loop_header_id,
    });

    b.select_block(loop_header_id);
    let t0 = b.new_temp(bool_ty());
    b.move_to(t0, IrOperand::Const(IrConst::Bool(true)));
    b.terminate(IrTerminator::CondBr {
        cond: temp(0),
        then_b: loop_body_id,
        else_b: exit_id,
    });

    b.select_block(loop_body_id);
    b.terminate(IrTerminator::Br {
        target: loop_header_id,
    });

    b.select_block(exit_id);
    b.terminate(IrTerminator::Ret { value: None });

    let func = b.finish();
    let cfg = &func.cfg;

    let mut blocks_iter = func.blocks.keys();
    let entry_id = blocks_iter.next().unwrap();
    let exit_id = blocks_iter.skip(2).next().unwrap();

    // Entry should be first in RPO
    assert_eq!(cfg.block_ids[0], *entry_id);

    // Exit should be last in RPO (it's the sink and comes after the loop)
    assert_eq!(cfg.block_ids[3], *exit_id);

    // The loop header and body should come between entry and exit
    // This validates that RPO correctly handles back edges
    let entry_idx = cfg
        .block_ids
        .iter()
        .position(|&id| id == *entry_id)
        .unwrap();
    let exit_idx = cfg.block_ids.iter().position(|&id| id == *exit_id).unwrap();
    assert!(entry_idx < exit_idx);
}
